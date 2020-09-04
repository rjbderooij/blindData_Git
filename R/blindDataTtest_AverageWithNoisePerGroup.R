#' Suitable for t-test:
#' 
#' For each group separately this function draws 1 value from distribution of Y 
#' from total data and averages that random noise and the original score per case
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictor # name of predictor, for example: "expert"
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords average with noise per group, ttest
#' @importFrom stats rnorm sd

blindDataTtest_AverageWithNoisePerGroup <- function(df_original, 
                                                    y, 
                                                    predictor, 
                                                    update_labels = TRUE){
  
  # Step 1: Create basis for blinded dataset with Y as first column
  df_blindAverageWithNoisePerCase <- df_original[,c(y, predictor)]
  
  # Step 2: Add noise per group 
  df_AverageWithNoisePerGroup <- 
    do.call(
      rbind, 
      lapply(
        split(df_original, interaction(df_original[,(predictor)])), 
        function(x){ 
          # For each group:
          # draw 1 value from distribution of Y from total data
          # and average random noise and original score per case
          x[[y]] <- 
            (x[,y] +                          # original
               rnorm(n = 1,                   # random noise per group
                     mean(df_original[,y]),   # based on total M & SD
                     sd(df_original[,y]))
            )/2  
          x 
        }
      )
    )
  
  # Step 3: Update Y label from: [name] to BLIND_ANPG_[name] 
  if(update_labels){
    names(df_AverageWithNoisePerGroup)[1] <- paste0("BLIND_ANPG_", y)
  }
  
  # Return df
  return(df_AverageWithNoisePerGroup)
  
} # End blindDataTtest_AverageWithNoisePerGroup

# blindDataTtest_AverageWithNoisePerGroup(df_original = df_sim_ttest,
#                                         predictor = "expert",
#                                         y = "score") 
