#' Suitable for regression:
#' This function creates blinded component residual plots
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @keywords Component Residual plot, Regression
#' @importFrom stats lm runif resid
#' @importFrom tidyselect all_of
#' @importFrom car crPlots
#'


plotRegression_ComponentResidual = function(df_original,
                                            y,
                                            predictors){

  ### Output:
  # Component residual plot (a.k.a. partial residual plot)
  #
  ### Steps:
  # 0) Remove ordering of predictors in dataset and scale variables to hide
  #    x-axis values that might reveal norminal or ordinal predictors
  # 1) Run original linear regression model and save output
  # 2) Call crPlots from the "car" package
  #

  # Step 0: randomize the variable order and rescale all variables
  df_original = df_original[, c(y, sample(predictors, replace = F))]
  df_original = as.data.frame(scale(df_original))


  # Step 1: Run original linear regression model to extract studres-values later
  lm_original = lm(paste0(y, " ~ ."), data = df_original)

  # Step 2: Call the component residual plot
  car::crPlots(lm_original,
               id = F ,
               grid = F,
               col = "white", # removes residual points so the nominal values are hidden
               xlab = NA,
               ylab = paste("Comp. + Resid. plot", y))

} # plotRegression_ComponentResidual

# plotRegression_ComponentResidual(df_original = df_sim_reg,
#                                  y = "sickleave",
#                                  predictors = c("gender",
#                                                 "general_health",
#                                                 "stress_at_work",
#                                                 "var_of_work_ac"))
