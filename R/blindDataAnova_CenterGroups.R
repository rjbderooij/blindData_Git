#' Suitable for ANOVA:
#' The function CenterGroups deducts, for each group, the average dependent 
#' variable value (Y) #' from each original dependent variable value (Y) 
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param update_labels # if TRUE update labels to BLIND_[abbrevation]_[name]
#' @keywords Center groups, ANOVA
#' 

blindDataAnova_CenterGroups <- function(df_original, 
                                        y, 
                                        predictors,
                                        update_labels = TRUE){
  
  # Step 1: Create basis for blidned dataset with Y as first column
  df_blindAverageWithNoisePerGroup <- df_original[,c(y, predictors)]
  
  # Step 2: Sample random value from distribution with mean of mean(Y) and 
  # sd of sd(Y) per group where each group is one of the possible combinations
  # of the predictors and average the values
  df_blindCenterGroups <- 
    do.call(
      rbind, lapply(
        # Split df in all possible groups by the interaction between predictors
        split(df_original, interaction(df_original[,(predictors)])), 
        function(x){ 
          # Center group scores
          x[[y]] <- x[,y] - mean(x[,y])
          x
        }
      )
    )
  
  # Step 3:  Update Y label from: [name] to BLIND_CG_[name] (CG = CenterGroups)
  if(update_labels){names(df_blindCenterGroups)[1] <- paste0("BLIND_CG_", y)}
  
  # Delete rownames
  row.names(df_blindCenterGroups) <- NULL
  
  # Return df
  return(df_blindCenterGroups)
  
} # End blindDataAnova_AddNoise