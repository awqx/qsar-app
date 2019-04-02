# Packages =====

# Manually calling library on each package instead of lapply so that rsconnect
# picks up on the dependencies

# ChemmineR requires a different installation
# library("BiocManager")
# BiocManager::install("ChemmineR")

# BiocManager::repositories()
# getOption("repos")

# install.packages("caret")
# install.packages("ChemmineR")
# install.packages("Cubist")
# install.packages("data.table")
# install.packages("e1071")
# install.packages("earth")
# install.packages("gbm")
# install.packages("glmnet")
# install.packages("kernlab")
# install.packages("randomForest")
# install.packages("RCurl")
# install.packages("stringr")
# install.packages("tidyverse")
# install.packages("tools")
# install.packages("xlsx")
# install.packages("XML")
library("caret")
library("ChemmineR")
library("Cubist")
library("data.table")
library("e1071")
library("earth")
library("gbm")
library("glmnet")
library("kernlab")
library("randomForest")
library("RCurl")
library("stringr")
library("tidyverse")
library("tools")
library("xlsx")
library("XML")

library("shiny")
library("shinythemes")
library("rsconnect")


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
