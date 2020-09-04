#' Suitable for t-test:
#' 
#' For each group separately this function centers Y scores per group (i.e., 
#' substracts the mean of each group from the Y scores of each case in that group)
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords center groups, ttest

blindDataTtest_CenterGroups <- function(df_original, 
                                        y, 
                                        predictor,
                                        update_labels = TRUE){
  
  # Step 1: Create basis for blinded dataset with Y as first column
  df_blindAverageWithNoisePerCase <- df_original[,c(y, predictor)]
  
  # Step 2: Center per group combination
  df_blindCenterGroups = 
    do.call(
      rbind, 
      lapply(
        split(df_original, interaction(df_original[,(predictor)])), 
        function(x){ 
          # Subtract mean of Y from each value of Y 
          x[[y]] <- x[,y] - mean(x[,y])
          x
        }
      )
    )
  
  # Step 3: Update Y label from: [name] to BLIND_CG_[name] (CG = CenterGroups)
  if(update_labels){names(df_blindCenterGroups)[1] <- paste0("BLIND_CG_", y)}
  
  # Return df
  return(df_blindCenterGroups)
  
} # End blindDataTtest_CenterGroups



# blindDataTtest_CenterGroups(df_original = df_sim_ttest,
#                             predictor = "expert",
#                             y = "score") 
