#' Suitable for regression:
#' This function creates a plot for hat-values
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param hat_cutoff_min = 2 # horizontal line in plot at selected value x mean
#' @param hat_cutoff_max = 3 # horizontal line in plot at selected value x mean
#' @keywords Hatvalues, Regression
#' @importFrom stats lm runif resid hatvalues
#' @importFrom graphics plot abline text
#'


plotRegression_Hatvalues <- function(df_original,
                                     y,
                                     predictors,
                                     hat_cutoff_min = 2,
                                     hat_cutoff_max = 3){

  ### Output:
  # Hat value plot with numbers indicating the observation indices that
  # exceed the cut-offscore of (1+k)/n
  #
  ### Steps:
  # 1) Save original dataframe
  # 2) Run original linear regression model and save output
  # 3) Extractstats::hatvalues
  # 4) Save max hatvalue (for plotting purposes)
  # 5) Calculate cutoff score (1+k)/n
  # 6) Construct the plot
  #

  # Step 1: Save original dataframe with Y as first column
  df_original = df_original[, c(y, predictors)]

  # Step 2: Run original linear regression model to extract hat-values later
  lm_original = lm(paste0(y, " ~ ."), data = df_original)

  # Step 3: Extract stats::hatvalues
  lm_hat = hatvalues(lm_original)

  # Step 4: Save max hatevalue
  lm_hat_max = max(lm_hat)

  # Step 5: Calculate cutoff score
  lm_hat_mean = mean(lm_hat) # is equal to (1 + number of predictors) / number of cases: (1+k)/n
  lm_hat_cutoffs = c(hat_cutoff_min, hat_cutoff_max)*lm_hat_mean

  # Step 6: Construct the plot
  graphics::plot(lm_hat,
       # y-limit = max of c(0, the max value in dataset, or the max cut-off limit)
       ylim = c(0, max(lm_hat_max,lm_hat_cutoffs)),
       xlab = "Observation indices",
       ylab = "Hat-values",
       main = "Hat-values"
  )
  graphics::abline(h = c(hat_cutoff_min,hat_cutoff_max)*lm_hat_mean, lty=2)

  # Display text next to the cases that exceed the minimal hat-value cutoff
  graphics::text(x = as.numeric(names(lm_hat[lm_hat > min(lm_hat_cutoffs)])),
       y = lm_hat[lm_hat > min(lm_hat_cutoffs)],
       labels = names(lm_hat[lm_hat > min(lm_hat_cutoffs)]),
       pos = 4, offset = 0.25)
} # plotRegression_Hatvalues
