#' Suitable for t-test:
#' The function "simulateDataRegression" simulates correlational simulates 
#' experimental data for a 2 group t-test analysis. The function returns a 
#' data.frame "df_sim_ttest" with two variables: one numeric dependent 
#' variable "score" and one independent factor variables "expert". By default,
#' the function simulates 50 (cell.size = 50) cases per group; with 2 groups 
#' this yields 100 cases. This simulation is made to resemble a dataset from
#' a psychological study. 
#' from a psychological study.
#' @param cell.size # number of cases to simulate per cell (i.e., group
#' combination)
#' @keywords Simulate data, ttest
#' @importFrom stats lm runif resid rbinom
#' @return df_sim_ttest = cell.size*4 length data.frame with variables in two 
#' colums:
#`  - score (numeric)
#`  - expert (factor)
#' @export

simulateDataTtest <- function(cell.size = 50){
  
  # Step 1: create dataframe
  df_sim_ttest <- data.frame(
    # a) Draw values at random for each group
    score = c(stats::rnorm(cell.size, mean = 3, sd = 1),   # low  expert
              stats::rnorm(cell.size, mean = 4.5, sd = 1)), # high expert
    
    # b) assign labels to expert variable
    expert = rep(c("low", "high"), 
                 each = cell.size,
                 length = 2*cell.size))
  
  # Step 2: set variables to factors: expert, conflict
  df_sim_ttest$expert = as.factor(df_sim_ttest$expert)
  
  return(df_sim_ttest)
} # End simulateDataTtest

# df_sim_ttest <- simulateDataTtest()
 