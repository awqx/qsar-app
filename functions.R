# Packages =====

# Manually calling library on each package instead of lapply so that rsconnect
# picks up on the dependencies

# ChemmineR requires a different installation
if (!require('BiocManager')) install.packages('BiocManager')
BiocManager::install("ChemmineR")

# BiocManager::repositories()
# getOption("repos")

if (!require("earth")) {
  install.packages(
    c(
      "Cubist",
      "e1071",
      "earth",
      "extrafont",
      "gbm",
      "glmnet",
      "kernlab",
      "rcdk",
      "ReacTran",
      "rsconnect",
      "shinythemes",
      "slickR",
      "svglite",
      "xlsx"
    )
  )
}
library("caret")
library("Cubist")
library("data.table")
library("devtools")
library("DT")
library("e1071")
library("earth")
library("extrafont")
library("gbm")
library("glmnet")
library("kernlab")
library("randomForest")
library("rcdk")
library("RCurl")
library("ReacTran")
library("rlang")
library("stringr")
library("tidyverse")
library("tools")
library("xlsx")
library("XML")
library("shiny")
library("shinythemes")
library("slickR")
library("tools")
library("rsconnect")

# loadfonts()

# Variables =====

# Alpha-CD

alpha.fill <- readRDS("./qsar/alpha/fill.RDS")
alpha.models <- readRDS("./qsar/alpha/models.RDS")
alpha.pp <- readRDS("./qsar/alpha/pp.settings.RDS")
alpha.vars <- readRDS("./qsar/alpha/vars.RDS")

# Beta-CD

beta.fill <- readRDS("./qsar/beta/fill.RDS")
beta.models <- readRDS("./qsar/beta/models.RDS")
beta.pp <- readRDS("./qsar/beta/pp.settings.RDS")
beta.vars <- readRDS("./qsar/beta/vars.RDS")

# Explore 
# FDA database

fda.explore <- readRDS("./explore/fda-pred.RDS")

# Pre-processing =====

    # Fill-in -----

# Replaces empty values in a table of descriptors w/ averages from 
# the modeling data

desc.fill <- function(df, fill.df) {
  # This works, but may need to be replaced w/ apply
  for(i in 1:nrow(df)) {
    for (k in 1:ncol(df)) {
      if(is.na(df[i, k]))
        df[i, k] <- fill.df[colnames(df)[k]]
    }
  }
  return(df)
}

    # Pre-processing -----

# Selects variables, centers and scales the descriptors
# df: data.frame containing name of the guests in first column and PaDEL descriptors
# vars: vector containing names of the selected features
# pp.settings: preProcess object created by caret
# fill.df: a data frame to be sent to desc.fill to replace NA values

pp.desc <- function(df, vars, pp.settings, fill.df) {
  # Replacing dashes in the column names
  colnames(df) <- str_replace(colnames(df), "_", "\\.")
  colnames(df) <- str_replace(colnames(df), "-", "\\.")
  
  # Storing away the names for later
  guests <- df[ , 1]
  desc <- df %>% dplyr::select(., vars)
  
  # Replace Inf values w/ NAs to be filled in
  desc <- do.call(data.frame, 
                  lapply(desc, function(x)
                    replace(x, is.infinite(x), NA)))
  
  # Filling in NA values with averages
  desc <- desc.fill(desc, fill.df)
  
  # print(head(desc))
  # Pre-processing
  pp <- predict(pp.settings, cbind(guests, desc))
  
  return(pp)
}



# QSAR =====

    # Ensemble -----
# Parsing through the models, predicting 
# df contains guest, DelG
ensemble <- function(df, models) {
  # results <- df %>% select(., guest, DelG)
  # desc <- df %>% select(., -guest, -DelG)
  
  results <- df %>% select(., guest)
  desc <- df %>% select(., -guest)
  
  for(i in 1:length(models)) {
    # message(names(models[i]), " starting")
    if(str_detect(names(models)[i], "gbm"))
      pred <- predict(models[[i]], desc, n.trees = 500) %>%
        data.frame()
    else
      pred <- predict(models[[i]], desc) %>%
        data.frame()
    # print(pred)
    colnames(pred) <- names(models)[i]
    results <- data.frame(results, pred)
    # message(names(models[i]), " completed")
  }
  # Creating an ensemble column that takes the averages of the QSARs
  results <- results %>%
    mutate(ensemble = rowMeans(results[ , -1]))
  return(results)
}

    # Applicability domain -----

# Finds standard deviation for a single descriptor
# Requires a vector or single column; returns num
find.sd.desc <- function(desc) {
  sd <- (desc[!is.na(desc)] - mean(desc, na.rm = T)) ^ 2 %>% sum()
  sd <- sqrt(sd / (length(desc) - 1))
  return(sd)
}

# Returns the newSk value based on Roy
# df: result of standardization (centering and scaling)

domain.num <- function(df) {
  
  # Initializing a vector of results
  newSk <- c(rep(0, nrow(df)))
  # Checking if first column is "guest
  if (class(df[, 1]) != "numeric") {
    guest <- df[, 1]
    df <- abs(df[ , -1])
    
    # Checking to see if single entry or multiple entries
    if(nrow(df) < 2) 
      result <- (df[ 1, ] +  1.28 * find.sd.desc(as.numeric(df[ 1, ]))) %>% 
        as.numeric() %>% mean()
    else
      result <-
        apply(df, 1, function(x)
          mean(as.numeric(x), na.rm = T) + 1.28 * find.sd.desc(as.numeric(x))) %>%
        as.data.frame()

    result <- data.frame(guest, result) %>%
      mutate(guest = as.character(guest))
    colnames(result)[2] <- "newSk"
  } else {
    df <- sapply(df, abs)
    result <- apply(df, 1, function(x)
      mean(as.numeric(x), na.rm = T) + 1.28 * find.sd.desc(as.numeric(x))) %>%
      as.data.frame()
    colnames(result)[1] <- "newSk"
  }
  max.ski <- apply(df, 1, max, na.rm = T)
  min.ski <- apply(df, 1, min, na.rm = T)
  result <- cbind(result, max.ski, min.ski)
  return(result %>% 
           mutate(domain = ifelse(result$max.ski < 3, "inside", 
                                  ifelse(result$min.ski > 3, "outside", 
                                         ifelse(result$newSk > 3, "outside", "inside")))))
}


# Cactus ====
    # Formatting -----

    # Read SDFs -----

# Slightly different from the 03.cactus.functions.R file
# instad of downloading to the local drive, attempting to read everything as a table

# Starting with a function to gather the SDFs

download.cactus <- function(guest) {
  guest.url      <- URLencode(guest, reserved = T)
  tryCatch({ 
    # destfile       <- paste0(path, "/", guest, ".SDF")
    # Chemical format must be parsed to match all the outputs from NCI cactus
    URL            <-
      paste0("https://cactus.nci.nih.gov/chemical/structure/",
             guest.url,
             "/file?format=sdf")
    sd.file <- readLines(URL) %>% data.frame() %>% sapply(as.character)
    # Renaming the SDF to be consistent with the guest name
    sd.file[1, 1] <- str_trim(guest)
    return(sd.file)
  },
  warning = function(warn) {
    message(paste0("Warning: URL does not exist - check for typos or try alternate name for ", guest))
    # URL            <- paste0(
    #   "https://cactus.nci.nih.gov/chemical/structure/",
    #   guest.url, "/file?format=sdf"
    # )
    # sd.file <- readLines(URL) %>% data.frame() %>% sapply(as.character)
    # # Renaming the SDF to be consistent with the guest name
    # sd.file[1, 1] <- str_trim(guest)
    return(data.frame())
  },
  error = function(err) {
    message(paste0("An error occurred - the structure file for ", guest, " could not be found"))
    return(data.frame())
  },
  finally = {
    message(guest, " processed")
  })
  # return(sd.file)
}


# And a version that can handle multiple guests
# Assuming that the guests are in a string separated by commas

download.cactus.multiple <- function(guest) {
  
  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  sdf <- do.call(rbind,
                 lapply(guest,
                        download.cactus))
  return(data.frame(sdf))
  
}

# Find the name of a guest in the SDF
# Returns a boolean - T if detected
# Currently unused due to being cannibalized by check.sdf

find.guest <- function(sdf, guest) {
  
  check <- sdf %>% str_detect(guest) %>% sum()
  return(check > 0)
  
}

# Checks to see if all guests are present in the SDF by using the names
# Returns a boolean vector corresponding to each guest

check.sdf <- function(sdf, guest) {
  
  sdf <- sdf[ , 1] %>% as.character()
  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  check <- lapply(guest, str_detect, string = sdf) %>% lapply(sum)
  
  # Return a boolean
  results <- check > 0
  names(results) <- guest 
  return(results)
  # # Return a dataframe
  # results <- ifelse(check > 0, "Found", "Not found")
  # return(data.frame(guest, results))
  
}

write.sdf <- function() {
  
}

# Downloads the SDF into a file rather than as an R object
# path: the folder to write the file, not the filename, should end in backslash

download.cactus.file <- function(guest, path) {
  
  download.cactus(guest) %>%
    write.table(., file = paste0(path, guest, ".SDF"), 
                quote = F, col.names = F, row.names = F)
  
}



# Molecules =====
    # Read SMILES
# A modification of download.cactus to get SMILES
download.smiles <- function(guest) {
  guest.url      <- URLencode(guest, reserved = T)
  tryCatch({ 
    # Chemical format must be parsed to match all the outputs from NCI cactus
    URL            <-
      paste0("https://cactus.nci.nih.gov/chemical/structure/",
             guest.url,
             "/smiles")
    # return the SMILES string
    return(as.character(readLines(URL, warn = F)))
  },
  warning = function(warn) {
    message(paste0("Warning: URL could not be read. Check for typos or try alternate name for ", guest))
    return(as.character(readLines(URL, warn = F)))
  },
  error = function(err) {
    message(paste0("Error: the structure file for ", guest, " could not be found"))
    return("")
  },
  finally = {
    message(guest, " processed")
  })
}

library(svglite)
library(slickR)
# Returns a vector of smiles
# Not yet used, but could be alternative to SDF because of smaller file size
download.smiles.multiple <- function(guest) {
  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  smiles <- sapply(guest, download.smiles)
  return(smiles)
}


# Renders the guests into different plots, each with four different molecules
# Plots are formatted as XML 
plot.smiles <- function(guest) {
  
  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  smiles <- sapply(guest, download.smiles)
  names(smiles) <- toTitleCase(names(smiles))
  mol <- parse.smiles(smiles)
  dep <- get.depictor(width = 400, height = 400, 
                      style = "cow")
  
  plots <- 
    lapply(
    # Creating the divisons
    1:ceiling(length(mol)/4), 
    function(x) {
      # Creating a page
      selected.mol <- (x-1)*4 + 1:4
      selected.mol <- selected.mol[!selected.mol > length(mol)]
      # Grabbing the 4 molecules to populate the plot
      mol.page <- mol[selected.mol]
      # Saving the names
      mol.names <- names(mol.page)
      # Renaming each to correspond to their location
      # upper left, upper right, bottom left, bottom right
      pos <- c("ul", "ur", "bl", "br") %>%
        head(length(selected.mol)) # so that names fit the molecules
      names(mol.page) <- pos
      
      # Creating the base graph
      xmlSVG({
        # windowsFonts(A = windowsFont("D-DIN"))
        plot(1, type="n", xlab="", ylab="", 
             xlim=c(0, 10), ylim=c(0, 10), 
             axes = F)
        
        for(i in 1:length(pos)) {
          switch(
            names(mol.page)[i],
            "ul" = {
              rasterImage(view.image.2d(mol.page[[i]], dep), 
                          0.5, 5, 4, 9.5, interpolate = T)
              text(1.5, 10, mol.names[i], family = "A")
            }, 
            "ur" = {
              rasterImage(view.image.2d(mol.page[[i]], dep), 
                          4.5, 5, 9, 9.5, interpolate = T)
              text(6.5, 10, mol.names[i], family = "A")
            },
            "bl" = {
              rasterImage(view.image.2d(mol.page[[i]], dep), 
                          0.5, 0, 5, 4.5, interpolate = T)
              text(1.5, 4.5, mol.names[i], family = "A")
            }, 
            "br" = {
              rasterImage(view.image.2d(mol.page[[i]], dep), 
                          4.5, 0, 9, 4.5, interpolate = T)
              text(6.5, 4.5, mol.names[i], family = "A")
            })
        }
      }, standalone = T)
    }
  )
  return(plots)
}

hash_encode_url <- function(url){
  gsub("#", "%23", url)
}
# plot.smiles("amoxicillin, ibuprofen, dimercaprol, mesalamine, acetaminophen, ibuprofen")
# smiles <- download.smiles.multiple("amoxicillin, acetaminophen, ibuprofen")
# mols <- parse.smiles(smiles)

plot.smiles <- function(guest) {

  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  smiles <- sapply(guest, download.smiles)
  names(smiles) <- toTitleCase(names(smiles))
  mol <- parse.smiles(smiles)
  dep <- get.depictor(width = 400, height = 400,
                      style = "cow")

  plots <-
    lapply(
      # Creating the divisons
      1:ceiling(length(mol)/4),
      function(x) {
        # Creating a page
        selected.mol <- (x-1)*4 + 1:4
        selected.mol <- selected.mol[!selected.mol > length(mol)]
        # Grabbing the 4 molecules to populate the plot
        mol.page <- mol[selected.mol]
        # Saving the names
        mol.names <- names(mol.page)
        # Renaming each to correspond to their location
        # upper left, upper right, bottom left, bottom right
        pos <- c("ul", "ur", "bl", "br") %>%
          head(length(selected.mol)) # so that names fit the molecules
        names(mol.page) <- pos

          # windowsFonts(A = windowsFont("D-DIN"))
          par(mai = c(0,0,0,0))
          plot(1, type="n", xlab="", ylab="",
               xlim=c(1, 9), ylim=c(1, 9), 
               axes = F, asp = 0.75, 
               main = "Molecule Structures", family = "A")

          for(i in 1:length(pos)) {
            switch(
              names(mol.page)[i],
              "ul" = {
                rasterImage(view.image.2d(mol.page[[i]], dep),
                            0.5, 5, 4, 9.5, interpolate = T)
                text(1.5, 10, mol.names[i], family = "A", cex = 1.5)
              },
              "ur" = {
                rasterImage(view.image.2d(mol.page[[i]], dep),
                            4.5, 5, 9, 9.5, interpolate = T)
                text(6.5, 10, mol.names[i], family = "A", cex = 1.5)
              },
              "bl" = {
                rasterImage(view.image.2d(mol.page[[i]], dep),
                            0.5, 0, 5, 4.5, interpolate = T)
                text(1.5, 4.5, mol.names[i], family = "A", cex = 1.5)
              },
              "br" = {
                rasterImage(view.image.2d(mol.page[[i]], dep),
                            4.5, 0, 9, 4.5, interpolate = T)
                text(6.5, 4.5, mol.names[i], family = "A", cex = 1.5)
              })
          }
          recordPlot()
      }
    )
  return(plots)
}

# plot.smiles("amoxicillin, ibuprofen, dimercaprol, mesalamine, acetaminophen, ibuprofen")
plot.smiles <- function(guest) {

  guest <- strsplit(guest, ",") %>% lapply(str_trim) %>% unlist()
  smiles <- sapply(guest, download.smiles)
  names(smiles) <- toTitleCase(names(smiles))
  mol <- parse.smiles(smiles)
  dep <- get.depictor(width = 400, height = 400,
                      style = "cow")

  plots <-
    lapply(
      # Creating the divisons
      1:ceiling(length(mol)/3),
      function(x) {
        # Creating a page
        selected.mol <- (x-1)*3 + 1:3
        selected.mol <- selected.mol[!selected.mol > length(mol)]
        # Grabbing the 4 molecules to populate the plot
        mol.page <- mol[selected.mol]
        # Saving the names
        mol.names <- names(mol.page)
        # windowsFonts(A = windowsFont("D-DIN"))
        par(mfrow = c(1, 3), mai = c(0, 0, 0, 0), 
            cex.main = 1.75)
        for(i in 1:length(mol.page)) {
          plot(1, type="n", xlab="", ylab="",
               xlim=c(1, 9), ylim=c(1, 9), 
               asp = 0.75, axes = F)
          rasterImage(view.image.2d(mol.page[[i]], dep),
                      0, 0, 10, 10, interpolate = T)
          text(4.5, 9.5, mol.names[i], family = "A", 
               cex = 2.5)
        }
        recordPlot()
      }
    )
  return(plots)
}
# 

# Release ====

# Sourced from Edgardo Rivera-Delgado (ERD) from 
# https://github.com/eriveradelgado/ODE_Practice

# assuming dG is in kj/mol
dg.to.ka <- function(dg) {
  # exp(n) = e^n
  joules <- dg * 1000
  return (exp(joules / (-8.314 * 298)))
}

affinity <- function(t, state , ...){
  
    # LIGAND + host <-> COMPLEX (LIGAND:host)
    
    # The LIGAND is free to diffuse along the vertical length (N) of the polymer 
    # cylinder. The host remains constant as it is part of the polymer cylinder 
    # matrix. The LIGAND can reversibly bind to the the host to form a COMPLEX. 
    # Only the free LIGAND diffuses into the liquid media and is called the RELEASE.
    
    
    # Extracting values passed into the function through the state argument
    LIGAND     <- state[1:N]
    COMPLEX    <- state[(N+1):(2*N)]
    RELEASE    <- state[2*N+1]
    
    # Initializing the derivatives vectors
    dLIGAND    <- rep(0, times = N)
    dCOMPLEX   <- rep(0, times = N)
    dRELEASE   <- 0
    Rb         <- rep(0, times = N)
    
    # Method of Lines  
    ## Binding process to host
    for(i in 1:(N-1)){
      Rb[i]        <- p1 * LIGAND[i] * (p3 - COMPLEX[i]) - COMPLEX[i] 
    }
    ## Diffusion process of the ligand 
    
    dLIGAND[1]   <- p2 * (LIGAND[2] - LIGAND[1]) / (delta^2) - Rb[1]
    
    ## This layer represents the polymer cylinder to liquid media interface
    
    dLIGAND[N-1] <- p2 * (-2 * LIGAND[N-1] + LIGAND[N-2]) / (delta^2) - Rb[N-1]
    
    ## These are the calculations for the layers in between top and center
    
    for(i in 2:(N-2)){
      dLIGAND[i]   <- 
        p2 * (LIGAND[i + 1] - 2 * LIGAND[i] + LIGAND[i-1]) / (delta^2) - Rb[i]
    }
    
    ## Change in COMPLEX through the polymer cylinder
    
    dCOMPLEX     <- Rb
    
    ## LIGAND exiting into the liquid environment
    dRELEASE     <- -  p2 * (LIGAND[N] - LIGAND[(N-1)]) / (delta^2) 
    
    dudt         <- c(dLIGAND, dCOMPLEX, dRELEASE)
    
    ncall <<- ncall + 1 
    
    return(list(dudt)) 
   
}

# affinity <- function(t, state, ...) {
#     # extracting states
#     ligand  <- state[1:nstep]
#     complex <- state[(nstep+1):(2*nstep)]
#     media <- state[2*nstep + 1]
#     
#     # initializing partial derivatives
#     dligand  <- rep(0, nstep)
#     dcomplex <- rep(0, nstep)
#     dmedia   <- 0
#     rb       <- rep(0, nstep)
#     
#     # MOL
#       # Binding of ligand to host
#     for(i in 1:(nstep - 1)) {
#       rb[i] <- p1 * ligand[i] * (p3 - complex[i]) - complex[i]
#     }
#     
#      # Diffusion of ligand through media
#     dligand[1] <- p2 * (ligand[2] - ligand[1]) / (delta^2 )- rb[1]
#     
#       # polymer-media interface
#     dligand[nstep-1] <- p2 * (-2*ligand[nstep-1] + ligand[nstep-2]) / (delta^2)- rb[nstep-1]
#     
#       # layers between the interface and the center
#     for(i in 2:(nstep-2)) {
#       dligand[i] <- 
#         p2 * (ligand[i+1] - 2*ligand[i] + ligand[i-1]) / (delta^2)- rb[i]
#       
#         # Change in [complex] through polymer
#       dcomplex <- rb
#         
#         # media: amount of ligand exiting into media
#       dmedia <- -p2 * (ligand[nstep] - ligand[nstep-1]) / (delta^2)
#       dudt <- c(dligand, dcomplex, dmedia)
#       
#       ncall <<- ncall + 1
#       
#       return(list(dudt))
#     
#   }
# }
