#' Suitable for ANOVA:
#' The function "blindDataAnova_ScramblePredictors" re-labels at random the 
#' grouping variables and then restores the original ordering by label name. 
#' Keep in mind that the factor labels (e.g., low/high) still remain, so the 
#' blinded dataset can still be traced back to the original variable (e.g., 
#' low/high is for expert variable, and conflict/no conflict is for conflict 
#' variable). Consider blindDataAnova_MaskGroups in combination with this 
#' method to prevent this.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Scramble predictors, ANOVA
#' @importFrom dplyr %>% select 
#' @importFrom tidyselect all_of
#' 

blindDataAnova_ScramblePredictors <- function(df_original, 
                                              y, 
                                              predictors,
                                              update_labels = TRUE){
  
  # Step 1: Check that there is more than 1 predictor (else no scrambling)
  if(length(predictors) == 1){
    print("Cannot scramble predictor variables if there is only one predictor")
    stop()
  }
  
  # Step 2: Reorder original dataframe with Y in first column
  df_original <- df_original %>% select(all_of(y), all_of(predictors))
  
  # Step 3: Create new dataset of predictors without outcome variable
  df_blindScramblePredictors <- df_original %>% select(all_of(predictors))
  
  # Step 4: Sample without replacmeent from the index-number of columns (e.g.,
  # 1,2,3,4) a new order of columns (e.g., 4,2,3,1)
  df_blindScramblePredictors <- 
    df_blindScramblePredictors %>% 
    select(
      sample(
        ncol(df_blindScramblePredictors), 
        replace = F)
    )
  
  # Step 5: cbind the original value of Y to the blinded predictor df
  df_blindScramblePredictors =
    data.frame(df_original %>% select(y), 
               df_blindScramblePredictors)
  
  # Step 6: Replace names of random-column df with original variable names 
  # (that still haave the original ordering)
  # NOTE that y was on same position: [1]
  if(update_labels){
    names(df_blindScramblePredictors) =
      c(names(df_original %>% select(y)), # Y 
        paste0("BLIND_SP_", names(df_original)[-1])) # X (-1 was Y)
  }
  
  # Return
  return(df_blindScramblePredictors)
} # End blindDataAnova_ScramblePredictors