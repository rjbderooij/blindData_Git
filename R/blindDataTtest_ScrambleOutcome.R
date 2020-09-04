#' Suitable for t-test:
#' 
#' This function re-assigns at random the factor levels for each predictor.
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords scramble groups, ttest
#' @importFrom dplyr %>% mutate_if select 
#' @importFrom tidyselect all_of

blindDataTtest_ScrambleOutcome <- function(df_original, 
                                           y, 
                                           predictor,
                                           update_labels = TRUE){
  
  # Step 1: Clone original dataframe with Y in first column
  # df_blindScrambleOutcome <- df_original %>% select(all_of(y), all_of(predictor))
  df_blindScrambleOutcome <- df_original[, c(y, predictor)]
  
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
} # End blindDataTtest_ScrambleOutcome

# blindDataTtest_ScrambleOutcome(df_original = df_sim_ttest,
#                                predictor = "expert",
#                                y = "score")


