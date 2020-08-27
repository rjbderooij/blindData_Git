#' Suitable for regression:
#' The function "simulateDataRegression" simulates correlational data for
#' regression analysis. The function returns a data.frame "df_sim_reg" with
#' five numeric variables:  one dependent variable "sickleave" (interval) and
#' four independent variables "gender", "general_health", "stress_at_work", and
#' "var_of_work_ac" (abbr. variety of work activities). By default, the function
#' simulates 85 (n = 85) cases. This simulation is made to resemble a dataset
#' from a psychological study.
#' @param n # number of cases to simulate
#' @keywords Average with noise per case, Regression
#' @importFrom stats lm runif resid rbinom
#'
### Output:
# df_sim_reg = n*5 dimensional data.frame with numeric variables in five colums:
# - sickleave
# - gender
# - general_health
# - stress_at_work
# - var_of_work_ac
#
### Steps:
# 1) define intercept
# 2) define slopes
# 3) simulate random indepdent variable values from binomial or from truncated
#    normal distribution
# 4) simulate error values random normal distribution
# 5) simulate dependent variable scores according to linear model
# 6) recode impossible values
# 7) create dataset

simulateDataRegression <- function(n = 85){
  # Step1: define intercept
  b0 <- 12

  # Step 2: define slopes
  b1 <-  3
  b2 <- -2
  b3 <-  1.5
  b4 <- -0.5

  # set seed
  # set.seed(123)

  # Step 3: simulate independent variables
  gender         <- stats::rbinom(n = n, size = 1, prob = 0.5)
  general_health <- round(msm::rtnorm(n = n, mean = 4, sd = 2, lower = 1, upper = 7))
  stress_at_work <- round(msm::rtnorm(n = n, mean = 4, sd = 2, lower = 1, upper = 7))
  var_of_work_ac <- round(msm::rtnorm(n = n, mean = 4, sd = 2, lower = 1, upper = 7))

  # Step 4: simulate random noise
  e <- rnorm(n = n, mean = 0, sd = 4)

  # Step 5: simulate dependent variable scores according to linear model:
  sickleave <- round(b0 +
                       b1 * gender +
                       b2 * general_health +
                       b3 * stress_at_work +
                       b4 * var_of_work_ac +
                       e
  )

  # Step 6: set the negative values to zero as the variable can only be positive
  sickleave[sickleave < 0] <- 0

  # Step 7: create dataset correlational data
  df_sim_reg <- as.data.frame(cbind(sickleave,
                                    gender,
                                    general_health,
                                    stress_at_work,
                                    var_of_work_ac))

  # Output: df_sim_reg
  return(df_sim_reg)
} # End simulateDataRegression

# set.seed(123)
# df_sim_reg <- simulateDataRegression()
# str(df_sim_reg)
# head(df_sim_reg)
