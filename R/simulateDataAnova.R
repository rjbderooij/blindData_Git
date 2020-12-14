#' Suitable for ANOVA:
#' The function "simulateDataAnova" simulates experimental data for a 2x2 ANOVA
#' analysis. The function returns a data.frame "df_sim_anova" with three variables:  
#' one numeric dependent variable "score" and two independent factor variables 
#' "conflic" and "expert". By default, the function simulates 50 (cell.size = 50) 
#' cases per group; with 4 groups (i.e., 2x2 design) this yields 200 cases. This 
#' simulation is made to resemble a dataset from a psychological study. 

#' @param cell.size # number of cases to simulate per cell (i.e., group
#' combination)
#' @keywords Simulate data, ANOVA
#' @importFrom stats rnorm
#' @return df_sim_anova = cell.size*4 length data.frame with variables in two 
#' colums:
#`  - expert (factor: low, high)
#`  - conflict (factor: no conflict, conflict)
#' @export

simulateDataAnova <- function(cell.size = 50){
  
  # Step 1: create dataframe
  df_sim_anova <- data.frame(
    # a) Draw values at random for each group
    score = c(stats::rnorm(cell.size, mean = 3, sd = 1),   # low  EXP, no CON
              stats::rnorm(cell.size, mean = 4.5, sd = 1), # high EXP, no CON
              stats::rnorm(cell.size, mean = 3, sd = 1),   # low  EXP, CON
              stats::rnorm(cell.size, mean = 2.5, sd = 1)),# high EXP, CON
    # b) assign labels to expert variable
    expert = rep(c("low", "high"), each = cell.size, length = 4*cell.size),
    # c) assign labels to conflict variable
    conflict = rep(c("no conflict", "conflict"), 
                   each = cell.size*2, 
                   length = 4*cell.size))
  
  # Step 2: set variables to factors: expert, conflict
  df_sim_anova$expert = df_sim_anova$expert %>% as.factor
  df_sim_anova$conflict = df_sim_anova$conflict %>% as.factor
  
  return(df_sim_anova)
}
# End simulateDataAnova