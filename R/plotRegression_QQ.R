#' Suitable for regression:
#' This function creates a QQ norm plot for the residuals
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @keywords Hatvalues, Regression
#' @importFrom stats qqnorm qqline rstudent
#' @importFrom graphics plot abline text
#'
#


plotRegression_QQ <- function(df_original,
                              y,
                              predictors){

  ### Output:
  # QQ norm plot with numbers indicating the observation indices that
  # exceed the cut-offscore of k/(n-k-1)
  #
  ### Steps:
  # 1) Run original linear regression model and save output
  # 2) Construct the plot
  #

  # Step 1: Run original linear regression model to extract studres-values later
  lm_original = lm(paste0(y, " ~ ."), data = df_original)

  # Construct plot
  stats::qqnorm(rstudent(lm_original), # extract studentized residuals
                pch = 1,
                frame = T,
                ylab = "Studentized Residuals",
                main = " QQ Plot")
  stats::qqline(rstudent(lm_original), col = "steelblue", lwd = 2)
} # End plotRegression_QQ
