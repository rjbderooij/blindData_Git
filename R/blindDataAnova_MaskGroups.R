#' Suitable for ANOVA:
#' The function "blindDataAnova_MaskGroups" masks the label names for the 
#' independent variables.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Mask groups, ANOVA
#' @importFrom dplyr %>% mutate_if mutate_all
#' 

blindDataAnova_MaskGroups <- function(df_original, 
                                      y, 
                                      predictors, 
                                      update_labels = TRUE){
  
  # Step 1: Select only the grouping variables
  df_predictors <- df_original[,predictors]
  
  # Step 2: If grouping variables are not set as factor, convert into factors
  df_predictors <-  
    df_predictors %>% 
    mutate_if(is.character, as.factor)     
  
  # Step 3: (helper) Function that can mask factor labels
  maskLabels <- function(factor_variable){
    
    # a) Extract number of labels
    n_labels <- length(levels(factor_variable))
    
    # b) Assign a new condition + n[i] at random (so it's not alphabetical)
    # For example, a 2 factor "low/high" becomes "Condition 1/Condition 2" 
    levels(factor_variable) <- paste("Condition", 
                                     sample(1:n_labels, replace = F))
    
    # c) Return randomized levels of factor variable
    return(factor_variable)  
  }
  
  # Step 4: Replace labels of all variables in df_predictors
  for(i in 1:ncol(df_predictors)){
    df_predictors[,i] <- maskLabels(df_predictors[,i])
  }
  
  # Step 5: Rename the predictors if update_labels = TRUE
  if(update_labels){
    names(df_predictors) <- paste0("BLIND_MG_", names(df_predictors))
  }
  
  # Step 5: Create blinded df of the masked labels of predictors and Y
  # Conscious decision not to include variables that aren't blinded 
  # (so only output a dataset with blinded variables), 
  df_blindMaskGroups <- data.frame(df_original[,y], df_predictors)
  
  # Return
  return(df_blindMaskGroups)
} # End blindDataAnova_MaskGroups