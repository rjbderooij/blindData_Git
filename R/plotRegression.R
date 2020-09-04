#' Suitable for regression:
#' This function is a main function. blindDataRegression allows the user to
#' blind a dataset that is intended for regression
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param plot_cooksdistance # Cook's distances plot
#' @param plot_crplot # component residual plot
#' @param plot_hatvalues # hat-values plot
#' @param plot_qqplot # qq norm plot#'
#' @param plot_studonfitted # studentized residuals on fitted values plot
#' @param hat_cutoff_min = 2 # horizontal line in plot at selected value x mean
#' @param hat_cutoff_max = 3 # horizontal line in plot at selected value x mean
#' @param studres_cutoff_min = -2 # value below which cases gets highlighted
#' @param studres_cutoff_max =  2 # value above which cases gets highlighted
#' @param lowess = TRUE # fit lowess line
#' @param lowesscol = "blue" # color of lowess line
#' @keywords Average with noise per case, Regression
#'
#'
#' @export


plotRegression <- function(df_original,
                           y,
                           predictors,
                           plot_cooksdistance = TRUE,
                           plot_crplot = TRUE,
                           plot_hatvalues = TRUE,
                           plot_qqplot = TRUE,
                           plot_studonfitted = TRUE,
                           hat_cutoff_min = 2,
                           hat_cutoff_max = 3,
                           studres_cutoff_min = -2,
                           studres_cutoff_max = 2,
                           lowess = TRUE,
                           lowesscol = "blue"){


  ### Output:
  # Blinded dataset with indication in variable names "BLIND_[methodabbreviated]_"
  #



  # Plot Cooks distance
  if(plot_cooksdistance){
    plotRegression_CooksDistance(df_original,
                                 y,
                                 predictors)}


  # Make component residual plot
  if(plot_crplot){
    plotRegression_ComponentResidual(df_original,
                                     y,
                                     predictors)}

  # Plot hat-values
  if(plot_hatvalues){
    plotRegression_Hatvalues (df_original,
                              y,
                              predictors,
                              hat_cutoff_min,
                              hat_cutoff_max)}

  # Make QQplot
  if(plot_qqplot){
    plotRegression_QQ(df_original,
                      y,
                      predictors)}

  # Plot studentized residuals (Y) on fitted values (X)
  if(plot_studonfitted){
    plotRegression_StudentizedResidualsOnFittedValues(df_original,
                                                      y,
                                                      predictors,
                                                      studres_cutoff_min,
                                                      studres_cutoff_max)}
} # End plotDataRegression

# plotRegression(df_original = df_sim_reg,
#                y = "sickleave",
#                predictors = c("gender",
#                               "general_health",
#                               "stress_at_work",
#                               "var_of_work_ac"),
#                plot_cooksdistance = TRUE,
#                plot_crplot = F,
#                plot_hatvalues = F,
#                plot_qqplot = F,
#                plot_studonfitted = F)
