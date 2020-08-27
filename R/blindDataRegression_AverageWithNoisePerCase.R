#' Suitable for regression:
#' This function average with noise per case perturbs the dependent variable
#' by averaging the original value with a random value from a random normal
#' where the mean and sd are equal to the mean and sd of the entire dataset.
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Average with noise per case, Regression
#' @importFrom stats lm runif resid rnorm sd
#' @importFrom tidyselect all_of
#'
### Input:
# df_original = original dataset (i.e., unblinded)
# y = dependent variable
# predictors = predictor variables (a.k.a. independent variables)
# update_labels = TRUE updates labels from [name] to blind_AB_[name]
#                 FALSE retains original labels
#                 defaults to true to clearly indicate a changes to the dataset
### Output:
# df_blindAverageWithNoisePerCase = data.frame with pertubed Y
#
### Steps:
# 1) Reorder original dataframe with Y as first column
# 2) Clone original dataframe with Y as first column
# 3) Add noise to Y:
#    a) Sample random value from distribution with mean of mean(Y) and sd of sd(Y)
#    b) Replace original values
# 4) update Y label from: [name] to BLIND_ANPC_[name]
#

blindDataRegression_AverageWithNoisePerCase <- function(df_original,
                                                        y,
                                                        predictors,
                                                        update_labels = TRUE){

  # Step 1: Reorder original dataframe with Y in first column
  df_original = df_original[,c(y, predictors)]

  # Step 2: Create clone of df_original with Y as first variable
  df_blindAverageWithNoisePerCase <- df_original

  # Step 3a: Sample random value from distribution with mean(Y) and of sd(Y)
  noise <- stats::rnorm(nrow(df_original), mean(df_original[,y]), stats::sd(df_original[,y]))

  # Step 3b: Replace original values with new values
  df_blindAverageWithNoisePerCase[,y] <- rowMeans(cbind(df_original[,y], noise))

  # Step 5: Update labels
  if(update_labels){names(df_blindAverageWithNoisePerCase)[1] = paste0("BLIND_ANPC_", y)}

  # Return df
  return(df_blindAverageWithNoisePerCase)

} # End blindDataRegression_AverageWithNoisePerCase
