#' Suitable for ANOVA:
#' The main function blindDataAnova takes an input dataset and offers data 
#' blinding techniques to blind the data: (1) AddNoise, (2) AddBias, 
#' (3) CreateNew, (4) ScrambleOutcome, and (5) ScramblePredictors.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param blinding_method # name of blinding method
#' @param n_permutations # number of requested permuted datasets
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords blindData, ANOVA
#' @examples 
#' blindDataAnova(df_original = df_sim_anova, 
#' y = "score",
#' predictors = c("expert", "conflict"),
#' blinding_method = "AverageWithNoisePerCase"
#' ) 
#'  
#' @export

blindDataAnova <- function(df_original, 
                           y, 
                           predictors,
                           blinding_method = c("AverageWithNoisePerCase",
                                               "AverageWithNoisePerGroup",
                                               "CenterGroups",
                                               "MaskGroups",
                                               "ScrambleCells",
                                               "ScrambleGroups",
                                               "ScrambleOutcome",
                                               "ScramblePredictors"),
                           n_permutations = 6,
                           update_labels = TRUE){
  
  # Assumptions of the inputs are checked here
  # Errors thrown if problem is found
  if(
    all(
      # Check for df_original is of class data.frame
      if(class(df_original) != "data.frame"){
        print("Error: df_original is not a data.frame")},
      # Check if variables are numeric or logical
      if(!(is.numeric(df_original[,y]))){
        print("Error: Y variable is not numeric")},
      # Check if variables are numeric or logical
      if(!all(sapply(df_original[,predictors], is.factor))){
        print("Error: at least one predcitor is not a factor")},
      # Check if correct input of blinding method
      if(!blinding_method %in% c("AverageWithNoisePerCase",
                                 "AverageWithNoisePerGroup",
                                 "CenterGroups",
                                 "MaskGroups",
                                 "ScrambleCells",
                                 "ScrambleGroups",
                                 "ScrambleOutcome",
                                 "ScramblePredictors")){
        print("Error: Invalid blinding method")}
    )
  ){# End assumption of input checks
    
    # AverageWithNoisePerGroup
    if(blinding_method == "AverageWithNoisePerGroup"){
      return(
        blindDataAnova_AverageWithNoisePerGroup(df_original = df_original,
                                                y = y, 
                                                predictors = predictors,
                                                update_labels = update_labels)
      )
    } 
    
    # AverageWithNoisePerCase
    if(blinding_method == "AverageWithNoisePerCase"){
      return(
        blindDataAnova_AverageWithNoisePerCase(df_original = df_original,
                                               y = y, 
                                               predictors = predictors,
                                               update_labels = update_labels)
      )
    } 
    
    # CenterGroups
    if(blinding_method == "CenterGroups"){
      return(
        blindDataAnova_CenterGroups(df_original = df_original,
                                    y = y, 
                                    predictors = predictors,
                                    update_labels = update_labels)
      )
    } 
    
    # MaskGroups
    if(blinding_method == "MaskGroups"){
      return(
        blindDataAnova_MaskGroups(df_original = df_original, 
                                  y = y, 
                                  predictors = predictors,
                                  update_labels = update_labels)
      )
    } 
    
    # CreateNew
    if(blinding_method == "ScrambleCells"){
      return(
        blindDataAnova_ScrambleCells(df_original = df_original, 
                                     y = y,
                                     predictors = predictors,
                                     n_permutations = n_permutations)
      )
    } 
    
    # ScrambleGroups
    if(blinding_method == "ScrambleGroups"){
      return(
        blindDataAnova_ScrambleGroups(df_original = df_original, 
                                      y = y,
                                      predictors = predictors,
                                      update_labels = update_labels)
      )
    } 
    
    # ScrambleOutcome
    if(blinding_method == "ScrambleOutcome"){
      return(
        blindDataAnova_ScrambleOutcome(df_original = df_original, 
                                       y = y, 
                                       predictors = predictors,
                                       update_labels = update_labels)
      )
    } 
    
    # ScramblePredictors
    if(blinding_method == "ScramblePredictors"){
      return(
        blindDataAnova_ScramblePredictors(df_original = df_original, 
                                          y = y, 
                                          predictors = predictors,
                                          update_labels = update_labels)
      )
    } 
  }
} # End blindDataAnova


# blindDataAnova(df_original = df_sim_anova,
#                y = "score",
#                predictors = c("expert", "conflict"),
#                blinding_method = "AverageWithNoisePerCase")

# blindDataAnova(df_original = simulateDataAnova(),
#                y = "score",
#                predictors = c("expert", "conflict"),
#                blinding_method = "AverageWithNoisePerCase")

