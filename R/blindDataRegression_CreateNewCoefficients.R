#' Suitable for regression:
#' This function creates new coefficients by replacing the original
#' coefficients with a random value from a random uniform distribution between
#' (-0.5 and +.5) that is multiplied by a scaling factor that is calculated by
#' dividing the sd of the dependent variabel from the entire data by the sd
#' of the predictor.
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Create New Coefficients, Regression
#' @importFrom stats lm runif resid
#'


blindDataRegression_CreateNewCoefficients = function(df_original,
                                         y,
                                         predictors,
                                         update_labels = TRUE){


  ### Input:
  # df_original = original dataset (i.e., unblinded)
  # y = dependent variable
  # predictors = predictor variables (a.k.a. independent variables)
  # update_labels = TRUE updates labels from [name] to blind_AB_[name]
  #                 FALSE retains original labels
  #                 defaults to true to clearly indicate a changes to the dataset
  ### Output:
  ### Output:
  # df_blindCreateNewCoefficients = data.frame with pertubed Y
  #
  ### Steps:
  # 1) Save original dataframe with Y as first column
  # 2) Run original linear regression model and save output
  # 3) Clone original dataframe with Y as first column
  # 4) Create New Y:
  #    a) Extract original value of predictor X
  #    b) multiply with random number (runif(1
  # 5) Calculate Y by taking the sum of the biased predictor values and adding
  #    the original residual
  # 6) Maintain new Y, but reset predictors to original values (the new dataframe
  #    now has a pertubed Y with original predictor values
  # 7) Update Y label from: [name] to BLIND_CN_[name] (CN = CreateNewCoefficients)


  # Step 1: Reorder original dataframe with Y as first column
  df_original = df_original[, c(y, predictors)]

  # Step 2: Run original linear regression model to extract residuals later
  lm_original <- lm(paste0(y, " ~ ."), data = df_original)

  # Step 3: Create clone of df_original with Y as first column
  df_blindCreateNewCoefficients <- df_original

  # Step 4: Create New Y (XB)
  for(i in 1:(ncol(df_original)-1)){ #-1 for Y
    # Steps for each individual predictor:
    # a) Extract original value of predictor X
    # b) multiply with random number (runif(1, -0.5, 0.5))
    # c) multiply with SD(Y) / SD (X(i))
    df_blindCreateNewCoefficients[,1+i] =
      # Original value of predictor X(i)
      df_original[1+i] *
      # Random drawn from uniform distribution
      runif(1, -0.5, 0.5) *
      # Standard deviation Y / standard deviation X(i)
      (sd(df_original[,y]) / sd(df_original[,1+i]))
  }

  # Step 5: Calculate new sickleave (Y*)
  df_blindCreateNewCoefficients[,y] <- rowSums(df_blindCreateNewCoefficients[,-1]) + resid(lm_original)

  # Step 6: Maintain new Y; reset predictors to original values (Y + X matrix)
  df_blindCreateNewCoefficients <- cbind(df_blindCreateNewCoefficients[,y], df_original[,-1])

  # Step 7: Update_labels
  if(update_labels){names(df_blindCreateNewCoefficients)[1] =
    paste0("BLIND_CN_", names(df_original[, y]))}

  # Return
  return(df_blindCreateNewCoefficients)
} # End blindDataRegression_CreateNewCoefficients
