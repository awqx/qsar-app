# Packages =====

if(!require("pacman")) { 
  install.packages("pacman")
  library(pacman)
} else
  library(pacman)

p_load(caret,
       Cubist,
       e1071, 
       earth,
       gbm,
       glmnet,
       kernlab, 
       # pls, 
       randomForest, 
       tidyverse
)

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
  
  print(head(desc))
  
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
find.sd.desc <- function(data) {
  sd <- (data[!is.na(data)] - mean(data, na.rm = T)) ^ 2 %>% sum()
  sd <- sqrt(sd / (length(data) - 1))
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
    df <- sapply(df[, -1], abs)
    if(nrow(df) == 1)
      result <- mean(
        as.numeric(data[1, ], na.rm = T) + 1.28 * find.sd.desc(as.numeric(df[1, ])))
    else
      result <-
      apply(df, 1, function(x)
        mean(as.numeric(x), na.rm = T) + 1.28 * find.sd.desc(as.numeric(x))) %>%
      as.data.frame()
    result <- data.frame(guest, result) %>%
      mutate(guest = as.character(guest))
    colnames(result)[2] <- "newSk"
  } else {
    df <- sapply(df[ , -1], abs)
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
