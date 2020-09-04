#' Suitable for regression:
#' This function creates a plot for Cook's distance values
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @keywords Cooks distance, Regression
#' @importFrom stats lm runif resid cooks.distance
#' @importFrom graphics plot abline text
#'
plotRegression_CooksDistance <- function(df_original,
                                         y,
                                         predictors){


  ### Output:
  # Cooks distance plot with numbers indicating the observation indices that
  # exceed the cut-offscore of k/(n-k-1)
  #
  ### Steps:
  # 1) Run original linear regression model and save output
  # 2) Calculate cooksvalues
  # 3) Calculate max Cook's value (for plotting purposes)
  # 4) Calculate cutoff score (k/(n-k-1))
  # 5) Construct the plot
  #


  # Step 1: Run original linear regression model to extract cooks-values later
  lm_original <- lm(paste0(y, " ~ ."), data = df_original)

  # Step 2: Calculate cooksvalues and cutoff scores
  lm_cooks <- cooks.distance(lm_original)

  # Step 3: Calculate max Cook's value (for plotting purposes)
  lm_cooks_max <- max(lm_cooks)

  # Step 4:Calculate cutoff score (k/(n-k-1))
  lm_cooks_cutoff <-
    length(predictors) / (nrow(df_original)-length(predictors) - 1)

  # Step 5: Construct the plot
  graphics::plot(lm_cooks,
                 # y-limit = max of c(0, the max value in dataset, or the max cut-off lim)
                 ylim = c(0, max(lm_cooks_max,lm_cooks_cutoff)),
                 xlab = "Observation indices",
                 ylab = "Cooks Distance",
                 main = " Cooks Distance"
  )
  # horizontal line at cut-off score of 4/n-k-1
  graphics::abline(h = length(predictors) / (nrow(df_original)-length(predictors) - 1),
                   lty = 2)
  # Display text next to the cases that exceed the minimal cooks-value cutoff
  graphics::text(x = as.numeric(names(lm_cooks[lm_cooks > lm_cooks_cutoff])),
                 y = lm_cooks[lm_cooks > lm_cooks_cutoff],
                 labels = names(lm_cooks[lm_cooks > lm_cooks_cutoff]),
                 pos = 4, offset = 0.25)
} # plotRegression_CooksDistance

# plotRegression_CooksDistance(df_original = df_sim_reg,
#                              y = "sickleave",
#                              predictors = c("gender",
#                                             "general_health",
#                                             "stress_at_work",
#                                             "var_of_work_ac"))

