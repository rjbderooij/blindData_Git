#' Suitable for regression:
#' This function adds bias to coefficients by multiplying the original
#' coefficient with a random value from a random uniform distribution between
#' (-2 and +2)
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Add Bias to Coefficients, Regression
#' @importFrom stats lm runif resid
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
# df_blindAddBiasToCoefficients = data.frame with pertubed Y
#
### Steps:
# 1) Save original dataframe
# 2) Run original linear regression model and save output
# 3) Clone original dataframe
# 4) Add bias to predictor values:
#    a) Extract coef. from model and then
#    b) multiply with random number and then
#    c) multiply original predictor values
# 5) Calculate Y by taking the sum of the biased predictor values and adding
#    the original residual
# 6) Maintain new Y, but reset predictors to original values (the new dataframe
#    now has a pertubed Y with original predictor values
# 7) Update Y label from: [name] to BLIND_AB_[name] (AB = AddBiasToCoefficients)

blindDataRegression_AddBiasToCoefficients = function(df_original,
                                                     y,
                                                     predictors,
                                                     update_labels = TRUE){

  # Step 1: Reorder original dataframe with Y in first column
  df_original = df_original[,c(y, predictors)]

  # Step 2: Run original linear regression model
  lm_original = lm(paste0(y, "  ~ ", paste(predictors, collapse = "+")),
                   data = df_original)

  # Step 3: Create clone of df_original with Y as first variable
  df_blindAddBiasToCoefficients = df_original

  # Step 4: AddBiasToCoefficients to original predictor values (XB)
  for(i in 1:(ncol(df_original)-1)){
    # Steps for each individual predictor:
    # a) Extract original B
    # b) multiply B with random number (runif(1,-2,2))
    # c) multiply with original value
    df_blindAddBiasToCoefficients[,1+i] =
      lm_original[["coefficients"]][[1+i]] *
      runif(1, -2, 2) *
      df_original[1+i]
  }

  # Step 5: Calculate new sickleave (Y*)
  df_blindAddBiasToCoefficients[,y] <- rowSums(df_blindAddBiasToCoefficients[,-1]) + resid(lm_original)

  # Step 6:  Maintain new Y; reset predictors to original values (Y* en X matrix)
  df_blindAddBiasToCoefficients <- cbind(df_blindAddBiasToCoefficients[,y], df_original[,-1])

  # Step 7: update_labels
  if(update_labels){names(df_blindAddBiasToCoefficients)[1] = paste0("BLIND_AB_", names(df_original)[1])}

  # Output: df_blindAddBiasToCoefficients
  return(df_blindAddBiasToCoefficients)
} # End blindDataRegression_AddBiasToCoefficients
