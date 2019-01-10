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

    # Pre-processing -----

# Selects variables, centers and scales the descriptors
# df: data.frame 
# vars: vector containing names of the selected features
# 


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

# QSAR =====

    # Ensemble -----
# Parsing through the models, predicting 
# df contains guest, DelG
ensemble <- function(df, models) {
  results <- df %>% select(., guest, DelG)
  desc <- df %>% select(., -guest, -DelG)
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
