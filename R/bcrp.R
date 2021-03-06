#' Breast Cancer Recovery Project
#'
#' Data from a three-arm randomized controlled trial. Women with early-stage
#' breast cancer were randomly assigned to a nutrition intervention (\eqn{n} = 85),
#' an education intervention (\eqn{n} = 83) or standard care (\eqn{n} = 84).
#' They were measured before and after treatment. These data contain the baseline
#' measurement and the 9-month follow-up.
#'
#' @format A data frame with 252 observations on the following 14 variables:
#' \describe{
#'   \item{\code{physt1}}{physical functioning (from SF-36) at baseline.}
#'   \item{\code{cesdt1}}{depression score (CESD) at baseline.}
#'   \item{\code{physt3}}{physical functioning (from SF-36) at 9 months follow-up.}
#'   \item{\code{cesdt3}}{depression score (CESD) at 9 months follow-up.}
#'   \item{\code{negsoct1}}{negative social interaction at baseline.}
#'   \item{\code{uncomt1}}{unmitigated communion at baseline.}
#'   \item{\code{disopt1}}{dispositional optimism at baseline.}
#'   \item{\code{comorbid}}{number of comorbidities (e.g. diabetes, migraines, arthritis, or angina).}
#'   \item{\code{age}}{age at baseline.}
#'   \item{\code{wcht1}}{weight change since diagnosis: yes [1] or no [0].}
#'   \item{\code{nationality}}{Caucasian [1] or not [0].}
#'   \item{\code{marital}}{married [1] or not [0].}
#'   \item{\code{trext}}{treatment extensiveness index: lumpectomy without
#'     or with one form of adjuvant therapy (radiation or chemo) [-1.77],
#'     lumpectomy with radiation and chemotherapy [0.26], mastectomy without
#'     or with lumpectomy, and without or with one form of adjuvant therapy [0.56],
#'     mastectomy without or with lumpectomy, and radiation and chemotherapy [2.59]. }
#'   \item{\code{cond}}{experimental condition: nutrition [1], education [2] or standard care [3].}
#'  }
#'
#' @details IMPORTANT: for questions about these data contact Elise Dusseldorp: elise.dusseldorp@fsw.leidenuniv.nl.
#' @source The authors thank M.F. Scheier for making his data available.
#' @references If you use these data, please refer to:
#'    Scheier M.F., Helgeson V.S., Schulz R., et al. (2007).
#'    Moderators of interventions designed to enhance physical and
#'    psychological functioning among younger women with early-stage breast cancer.
#'    \emph{Journal of Clinical Oncology, 25}, 5710-5714.
#'
#'    An example of a complete analysis on these data using the \code{quint} package is given in:
#'    Dusseldorp, E., Doove, L., & Van Mechelen, I. (2016). Quint:
#'    An R package for the identification of subgroups of clients who differ in
#'    which treatment alternative is best for them. \emph{Behavior Research Methods,
#'    48}(2), 650-663. DOI 10.3758/s13428-015-0594-z.
#'
#'    An application of \code{quint} to these data is given in:
#'    Dusseldorp E. and Van Mechelen I. (2014). Qualitative interaction trees:
#'    a tool to identify qualitative treatment-subgroup interactions.
#'    \emph{Statistics in Medicine, 33}(2), 219-237. DOI: 10.1002/sim.5933.
"bcrp"
