#' Suitable for t-test:
#' 
#' For each case separately this function draws 1 value from distribution of Y 
#' from total data and averages that random noise and the original score 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords average with noise per case, ttest
#' @importFrom stats lm runif resid rnorm sd

blindDataTtest_AverageWithNoisePerCase <- function(df_original, 
                                                   y, 
                                                   predictor,
                                                   update_labels = TRUE){
  
  # Step 1: Create basis for blidned dataset with Y as first column
  df_blindAverageWithNoisePerCase <- df_original[,c(y, predictor)]
  
  # Step 2: Sample random value from distribution with mean of mean(Y) and sd of sd(Y)
  noise = rnorm(nrow(df_original), mean(df_original[,y]), sd(df_original[,y]))
  
  # Step 3: Replace original values with new values: mean(original and noise)
  df_blindAverageWithNoisePerCase[,y] <- (df_original[,y] + noise)/2
  
  # Step 4:  Update Y label from: [name] to BLIND_ANPC_[name] 
  if(update_labels){names(df_blindAverageWithNoisePerCase)[1] <- paste0("BLIND_ANPC_", y)}
  
  # Return df
  return(df_blindAverageWithNoisePerCase)
  
} # End blindDataTtest_AverageWithNoisePerCase

# blindDataTtest_AverageWithNoisePerCase(df_original = df_sim_ttest,
#                                        predictor = "expert",
#                                        y = "score") 
