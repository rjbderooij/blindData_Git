#' Suitable for ttest:
#' This function is a main function. blindDataTtest allows the user to
#' blind a dataset that is intended for regression
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param blinding_method # name of blinding method to apply to the dataset
#' user can input any of the following blinding methods
#' c("AverageWithNoisePerCase", 
#' "AverageWithNoisePerGroup",
#' "CenterGroups",
#' "MaskGroups",
#' "ScrambleGroups",
#' "ScrambleOutcome")
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords blindData, Ttest
#' @examples
#' blindDataTtest(
#' df_original = data.frame(sickleave = 1:10, var1 = as.factor(rep(c(0,1), each = 5))),
#' y = "sickleave",
#' blinding_method = "AverageWithNoisePerCase",
#' predictor = "var1"
#' )
#'
#' @export

blindDataTtest = function(df_original, 
                          y, 
                          predictor,
                          blinding_method = c("AverageWithNoisePerCase", 
                                              "AverageWithNoisePerGroup",
                                              "CenterGroups",
                                              "MaskGroups",
                                              "ScrambleGroups",
                                              "ScrambleOutcome"),
                          update_labels = TRUE){
  
  # Assumptions of the inputs are checked here
  # Errors thrown if problem is found
  if(
    all(
      # Check for df_original is of class data.frame
      if(class(df_original) != "data.frame"){
        print("Error: df_original is not a data.frame")},
      # Check if variables are numeric or logical
      if(!(df_original[,y] %>% is.numeric)){
        print("Error: Y variable is not numeric")},
      # Check if variables are numeric or logical
      if(!all(sapply(df_original[,predictor], is.factor))){
        print("Error: at least one predcitor is not a factor")},
      # Check if correct input of blinding method
      if(!blinding_method %in% c("AverageWithNoisePerCase", 
                                 "AverageWithNoisePerGroup",
                                 "CenterGroups",
                                 "MaskGroups",
                                 "ScrambleGroups",
                                 "ScrambleOutcome")){
        print("Error: Invalid blinding method")}
    )
  ){# End assumption of input checks
    
    # AverageWithNoisePerCase
    if(blinding_method == "AverageWithNoisePerCase"){
      return(
        blindDataTtest_AverageWithNoisePerCase(df_original = df_original,
                                               y = y, 
                                               predictor = predictor,
                                               update_labels = update_labels)
      )
    } 
    
    
    # AverageWithNoisePerGroup
    if(blinding_method == "AverageWithNoisePerGroup"){
      return(
        blindDataTtest_AverageWithNoisePerGroup(df_original = df_original,
                                                y = y, 
                                                predictor = predictor,
                                                update_labels = update_labels)
      )
    } 
    
    # CenterGroups
    if(blinding_method == "CenterGroups"){
      return(
        blindDataTtest_CenterGroups(df_original = df_original,
                                    y = y, 
                                    predictor = predictor,
                                    update_labels = update_labels)
      )
    } 
    
    # MaskGroups
    if(blinding_method == "MaskGroups"){
      return(
        blindDataTtest_MaskGroups(df_original = df_original, 
                                  y = y, 
                                  predictor = predictor,
                                  update_labels = update_labels)
      )
    } 
    
    # ScrambleGroups
    if(blinding_method == "ScrambleGroups"){
      return(
        blindDataTtest_ScrambleGroups(df_original = df_original, 
                                      y = y,
                                      predictor = predictor,
                                      update_labels = update_labels)
      )
    } 
    
    # ScrambleOutcome
    if(blinding_method == "ScrambleOutcome"){
      return(
        blindDataTtest_ScrambleOutcome(df_original = df_original, 
                                       y = y, 
                                       predictor = predictor,
                                       update_labels = update_labels)
      )
    } 
  }
} # End blindDataTtest
