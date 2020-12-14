#' Suitable for regression:
#' This function is a main function. blindDataRegression allows the user to
#' blind a dataset that is intended for regression
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param blinding_method # name of blinding method to apply to the dataset
#' user can input any of the following blinding methods
#' c("AverageWithNoisePerCase",
#' "AddBiasToCoefficients",
#' "CreateNewCoefficients",
#' "ScrambleOutcome",
#' "ScramblePredictors")
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords blindData, Regression
#' @examples
#' blindDataRegression(
#' df_original = data.frame(sickleave = 0:10, var1 = 100:110, var2 = 200:210),
#' y = "sickleave",
#' blinding_method = "AverageWithNoisePerCase",
#' predictors = c("var1", "var2")
#' )
#'
#' @export

blindDataRegression = function(df_original,
                               y,
                               predictors,
                               blinding_method = c("AverageWithNoisePerCase",
                                                   "AddBiasToCoefficients",
                                                   "CreateNewCoefficients",
                                                   "ScrambleOutcome",
                                                   "ScramblePredictors"),
                               update_labels = TRUE){


  ### Output:
  # Blinded dataset with indication in variable names "BLIND_[methodabbreviated]_"
  #
  ### Function assumes:
  # All variables are interval (i.e., no factors or characters)
  # Check for df_original is of class data.frame
  # Check if variables are numeric or logical
  # Check if correct input of blinding method


  # Assumptions of the inputs are checked here
  # Errors thrown if problem is found
  if(
    all(
      # Check for df_original is of class data.frame
      if(class(df_original) != "data.frame"){
        print("Error: df_original is not a data.frame")},
      # Check if variables are numeric or logical
      if(!all(apply(df_original, 2, is.numeric))){
        print("Error: some variable is not numeric or binairy")},
      # Check if correct input of blinding method
      if(!blinding_method %in% c("AverageWithNoisePerCase",
                                 "AddBiasToCoefficients",
                                 "CreateNewCoefficients",
                                 "ScrambleOutcome",
                                 "ScramblePredictors")){
        print("Error: Invalid blinding method")}
    )
  ){# End assumption of input checks

    # AddBiasToCoefficients
    if(blinding_method == "AddBiasToCoefficients"){
      return(
        blindDataRegression_AddBiasToCoefficients(df_original = df_original,
                                                  y = y,
                                                  predictors = predictors,
                                                  update_labels = update_labels)
      )
    }

    # AverageWithNoisePerCase
    if(blinding_method == "AverageWithNoisePerCase"){
      return(
        blindDataRegression_AverageWithNoisePerCase(df_original = df_original,
                                                    y = y,
                                                    predictors = predictors,
                                                    update_labels = update_labels)
      )
    }


    # CreateNewCoefficients
    if(blinding_method == "CreateNewCoefficients"){
      return(
        blindDataRegression_CreateNewCoefficients(df_original = df_original,
                                                  y = y,
                                                  predictors = predictors,
                                                  update_labels = update_labels)
      )
    }

    # ScrambleOutcome
    if(blinding_method == "ScrambleOutcome"){
      return(
        blindDataRegression_ScrambleOutcome(df_original = df_original,
                                            y = y,
                                            predictors = predictors,
                                            update_labels = update_labels)
      )
    }

    # ScramblePredictors
    if(blinding_method == "ScramblePredictors"){
      return(
        blindDataRegression_ScramblePredictors(df_original = df_original,
                                               y = y,
                                               predictors = predictors,
                                               update_labels = update_labels)
      )
    }
  }
} # End blindDataRegression

