#' Suitable for ANOVA:
#' The function "blindDataAnova_CreateNew" creates a new dependent variable by 
#' re-assigning at random the Y value of one participant to a different 
#' participant.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Scramble outcome, ANOVA
#' @importFrom dplyr %>% select 
#' @importFrom tidyselect all_of
#' 

blindDataAnova_ScrambleOutcome <- function(df_original, 
                                           y, 
                                           predictors,
                                           update_labels = TRUE){
  
  # Step 1: Clone original dataframe with Y in first column
  df_blindScrambleOutcome <- 
    df_original %>% select(all_of(y), all_of(predictors))
  
  # Step 2: Replace Y with randomly sampled without replacement from Y
  df_blindScrambleOutcome[,y] <- sample(df_blindScrambleOutcome[,y], 
                                        replace = F) 
  
  # Step 3: Update name of Y to indicate blindedness
  if(update_labels){
    names(df_blindScrambleOutcome)[1] = 
      paste0("BLIND_SO_", names(df_original)[1])
  }
  
  # Return
  return(df_blindScrambleOutcome)
} # End blindDataAnova_ScrambleOutcome