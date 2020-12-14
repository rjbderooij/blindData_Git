#' Suitable for ANOVA:
#' The function AverageWithNoisePerCase averages: each original dependent 
#' variable value (Y) and random noise that is drawn for each CASE from a 
#' distribution that is based on the mean(Y) and sd(Y).
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Average with noise per case, Regression
#' @importFrom stats rnorm
#' 

blindDataAnova_AverageWithNoisePerCase <- function(df_original, 
                                                   y, 
                                                   predictors,
                                                   update_labels = TRUE){
  
  # Step 1: Create basis for blidned dataset with Y as first column
  df_blindAverageWithNoisePerCase <- df_original[,c(y, predictors)]
  
  # Step 2: Sample random value from distribution with mean of mean(Y) 
  # and sd of sd(Y) of original data
  noise <- stats::rnorm(nrow(df_original), 
                        mean(df_original[,y]),
                        sd(df_original[,y]))
  
  # Step 3: Replace original values with average of original and noise
  df_blindAverageWithNoisePerCase[,y] = (df_original[,y] + noise)/2
  
  # Step 4:  Update Y label from: [name] to BLIND_ANPC_[name] 
  if(update_labels){names(df_blindAverageWithNoisePerCase)[1] <- 
    paste0("BLIND_ANPC_", y)}
  
  # Return df
  return(df_blindAverageWithNoisePerCase)
  
} # End blindDataAnova_AverageWithNoisePerCase