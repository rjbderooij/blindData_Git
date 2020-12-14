#' Suitable for ANOVA:
#' The function AverageWithNoisePerCase averages: each original dependent 
#' variable value (Y) and random noise that is drawn for each GROUP from a 
#' distribution that is based on the mean(Y) and sd(Y).
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)#' 
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Average with noise per group, ANOVA
#' @importFrom stats rnorm
#' 

blindDataAnova_AverageWithNoisePerGroup <- function(df_original, 
                                                    y, 
                                                    predictors,
                                                    update_labels = TRUE){
  
  # Step 1: Create basis for blidned dataset with Y as first column
  df_blindAverageWithNoisePerGroup <- df_original[,c(y, predictors)]
  
  # Step 2: Sample random value from distribution with mean of mean(Y) and 
  # sd of sd(Y) per group where each group is one of the possible combinations
  # of the predictors and average the values
  df_blindAverageWithNoisePerGroup = 
    do.call(
      rbind, lapply(
        # Split df in all possible groups by the interaction between predictors
        split(df_original, interaction(df_original[,(predictors)])), 
        function(x){ 
          # Create noise per group based on mean and sd of total data
          noise <- stats::rnorm(n = 1, 
                                mean(df_original[,y]), 
                                sd(df_original[,y]))
          # Average per case the noise with the original score
          x[[y]] <- (x[,y] + noise)/2
          x
        }
      )
    )
  
  # Step 3:  Update Y label from: [name] to BLIND_ANPG_[name] 
  if(update_labels){names(df_blindAverageWithNoisePerGroup)[1] <- paste0("BLIND_ANPG_", y)}
  
  # Delete rownames
  row.names(df_blindAverageWithNoisePerGroup) <- NULL
  
  # Return df
  return(df_blindAverageWithNoisePerGroup)
  
} # End blindDataAnova_AddNoise