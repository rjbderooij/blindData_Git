#' Suitable for ANOVA:
#' The function "blindDataAnova_ScrambleGroups" re-assigns at random the factor
#' levels for each predictor variable.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Mask groups, ANOVA
#' @importFrom dplyr %>% mutate_if 
#' 

blindDataAnova_ScrambleGroups <- function(df_original,
                                          y, 
                                          predictors, 
                                          update_labels = TRUE){
  
  # Step 1: Select only the grouping variables
  df_predictors <- df_original[,predictors]
  
  # Step 2: If grouping variables are not set as factor, convert  into factors
  df_predictors <- 
    df_predictors %>% 
    mutate_if(is.character, as.factor)
  
  # Step 3: Replace labels of all variables in df_predictors
  # (helper) Function that can randomize factor labels
  replaceLabels <- function(factor_variable){
    
    # a) Extract original labels
    original_labels <- levels(factor_variable)
    
    # b) Resample the factor levels at random without replacement
    # set.seed(4) # As an example that switches a 2 level factor
    levels(factor_variable) <- sample(original_labels, replace = F)
    
    # Return
    return(factor_variable)  
  } # End replaceLabels
  
  # c) Replace labels of all variables in df_predictors
  for(i in 1:ncol(df_predictors)){
    df_predictors[,i] <- replaceLabels(df_predictors[,i])
  }
  
  # Step 4: Rename the predictors to "BLIND_SG" if update_labels = TRUE
  if(update_labels){
    names(df_predictors) <- paste0("BLIND_SG_", names(df_predictors))
  }
  
  # Step 5: Create blinded df that is combination of the scambled predictors 
  # and Y. Conscious decision not to include all variables, as you want the 
  # researcher to have a blinded dataset only
  # If you give unblinded variables and they decide to include them later,  
  # they have partly unblinded data. 
  df_blindScrambleGroups <- data.frame(df_original[ ,y], df_predictors)
  
  # Return
  return(df_blindScrambleGroups)
} # End blindDataAnova_ScrambleGroups