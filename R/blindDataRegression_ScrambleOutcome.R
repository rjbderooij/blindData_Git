#' Suitable for regression:
#' This function reassigns the dependent variable at random.
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Scramble outcome, Regression
#' @importFrom stats lm runif resid
#' @importFrom tidyselect all_of
#'


blindDataRegression_ScrambleOutcome <- function(df_original,
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
  #
  ### Output:
  # df_blindCreateNew = data.frame with pertubed Y
  #
  ### Steps:
  # 1) Save original dataframe with Y as first column
  # 2) Replace Y with randomly sampled without replacement from Y
  # 3) Update Y label from: [name] to BLIND_SO_[name] (SO = ScrambleOutcome)



  # Step 1: Reorder original dataframe with Y in first column
  df_original = df_original[, c(y, predictors)]

  # Step 2: Replace Y with randomly sampled without replacement from Y
  df_blindScrambleOutcome <- df_original
  df_blindScrambleOutcome[,y] <- sample(df_blindScrambleOutcome[,y], replace = F)

  # Step 3: Set names to indicate blindedness: update_labels
  if(update_labels){
    names(df_blindScrambleOutcome)[1] =
      paste0("BLIND_SO_", names(df_original)[1])
  }

  # Return
  return(df_blindScrambleOutcome)
} # End blindDataRegression_ScrambleOutcome
