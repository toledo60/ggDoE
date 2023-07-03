#' Girder experiment
#'
#' An experiment (Narayanan and Adorisio, 1983) to compare four methods for predicting the shear strength for steel plate girders.
#' Data for nine girders in the form of the ratio of predicted to observed load for these procedures are given.
#' Each of the four methods was used to predict the strength of each of the nine girders.
#'
#' @format A tibble with 36 rows and 3 variables
#' \describe{
#' \item{girders}{A factor denoting one of the nine girders}
#' \item{method}{A factor denoting one of the four methods for predicting the shear strength for steel plate girders: Aarau, Karlsruhe,Lehigh, Cardiff}
#' \item{response}{The shear strength for steel plate girders}
#' }
#' @source Experiments:  Planning, Analysis, and Optimization, CFJ Wu, MS Hamada - Second edition.
#'
#'@examples
#' lm(response ~ method + girders, data = girder_experiment) |>
#' anova()
"girder_experiment"
