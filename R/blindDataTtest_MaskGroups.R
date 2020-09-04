#' Suitable for t-test:
#' 
#' The function "blindDataTtest_MaskGroups" masks the label names for the 
#' independent variables. This blinding works also if there are more than 2 
#' groups, but the function will throw a message that there are more than 2 
#' groups. For example, the variable "expert" has two factor labels: "low/high". 
#' After blinding, this becomes "Condition 1/Condition 2" 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords mask groups, ttest
#' @importFrom dplyr mutate_if select

blindDataTtest_MaskGroups <- function(df_original, 
                                      y, 
                                      predictor, 
                                      update_labels = TRUE){
  
  # Step 1: Select only the grouping variables
  df_predictor <- as.data.frame(df_original[,predictor])
  
  # Step 2: If grouping variable is not set as factor, convert into factors
  df_predictor <- mutate_if(df_predictor, is.character, as.factor)
  
  # Step 3: (helper) Function that can mask factor labels
  maskLabels <- function(factor_variable){
    
    # a) Extract number of labels and 
    n_labels <- length(levels(factor_variable))
    
    # throw a message if there are more than 2 groups
    if(n_labels>2){
      print("The predictor variable has more than 2 groups; t-test requires 
            2 groups")}
    
    # b) Assign a new condition + n[i] at random (so it's not alphabetical)
    # For example, a 2 factor "low/high" becomes "Condition 1/Condition 2" 
    levels(factor_variable) <- paste("Condition", 
                                     sample(1:n_labels, replace = F))
    
    # c) Return randomized levels of factor variable
    return(factor_variable)  
  }
  
  # Step 4: Replace labels of all variables in df_predictor
  for(i in 1:ncol(df_predictor)){
    df_predictor[,i] <- maskLabels(df_predictor[,i])
  }
  
  # Step 5: Rename the predictor if update_labels = TRUE
  if(update_labels){
    names(df_predictor) <- 
      paste0("BLIND_MG_", names(df_predictor))
  }
  
  # Step 5: Create blinded df of the masked labels of predictor and Y
  # Conscious decision not to include variables that aren't blinded 
  # (so only output a dataset with blinded variables), 
  df_blindMaskGroups <- data.frame(df_original[,y],
                                   df_predictor)
  
  # Return
  return(df_blindMaskGroups)
  
} # End blindDataTtest_MaskGroups

# blindDataTtest_MaskGroups(df_original = df_sim_ttest,
#                           y = "score",
#                           predictor = "expert")


