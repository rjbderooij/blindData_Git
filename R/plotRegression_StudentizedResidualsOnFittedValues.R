#' Suitable for regression:
#' This function creates a plot of studentized residuals on fitted values
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)#'
#' @param studres_cutoff_min = -2 # value below which cases gets highlighted
#' @param studres_cutoff_max =  2 # value above which cases gets highlighted
#' @param lowess = TRUE # fit lowess line
#' @param lowesscol = "blue # color of lowess line
#' @keywords studentized residuals plot, Regression
#' @importFrom stats lm runif resid fitted
#' @importFrom graphics plot abline lines text
#'


plotRegression_StudentizedResidualsOnFittedValues <- function(df_original,
                                                              y,
                                                              predictors,
                                                              studres_cutoff_min = -2,
                                                              studres_cutoff_max = 2,
                                                              lowess = TRUE,
                                                              lowesscol = "blue"){


  ### Output:
  # Cooks distance plot with numbers indicating the observation indices that
  # exceed the cut-offscore of k/(n-k-1)
  #
  ### Steps:
  # 1) Run original linear regression model and save output
  # 2) Calculate studresvalues
  # 3) Calculate max studresv alues (for plotting purposes)
  # 4) Extract fitted values of linear model
  # 5) Construct the plot


  # Step 1: Run original linear regression model to extract studres-values later
  lm_original = lm(paste0(y, " ~ ."), data = df_original)

  # Step 2: Extract studresvalues
  lm_studres = stats::rstudent(lm_original)

  # Step 3: Extract min/max studres values
  lm_studres_min = min(lm_studres)
  lm_studres_max = max(lm_studres)

  # Step 4: Extract fitted values of linear model
  lm_fitted = stats::fitted(lm_original)

  # step 5: Construct the plot
  graphics::plot(x = lm_fitted,
                 y = lm_studres,
                 # y-limit = max of c(0, the max value in dataset, or the max cut-off limit)
                 ylim = c(min(studres_cutoff_min, lm_studres_min),
                          max(lm_studres_max, studres_cutoff_max)),
                 xlab = "Fitted values",
                 ylab = "Studentized residuals",
                 main = " Studentized residuals (Y) on fitted values (X)"
  )
  graphics::abline(h = c(studres_cutoff_min,studres_cutoff_max), lty=2)

  # Display text next to the cases tstudres exceed the minimal or maximal studres-value cutoff
  graphics::text(x = lm_fitted[lm_studres <= studres_cutoff_min | lm_studres >= studres_cutoff_max],
                 y = lm_studres[lm_studres <= studres_cutoff_min | lm_studres >= studres_cutoff_max],
                 labels = names(lm_studres[lm_studres <= studres_cutoff_min | lm_studres >= studres_cutoff_max]),
                 pos = 4, offset = 0.25)

  # Fit lowess line
  if(lowess){lines(lowess(lm_fitted, lm_studres), col = lowesscol, lwd = 2)} # lowess line
} # End plotRegression_StudentizedResidualsOnFittedValues
