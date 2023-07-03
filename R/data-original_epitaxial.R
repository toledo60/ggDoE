#' Original epitaxial layer experiment
#'
#' One of the initial steps in fabricating integrated circuit (IC) devices is to grow an epitaxial layer on polished silicon wafers.
#' The wafers are mounted on a six-faceted cylinder (two wafers per facet), called a susceptor, which is spun inside a metal bell jar.
#' The jar is injected with chemical vapors through nozzles at the top of the jar and heated.
#' The process continues until the epitaxial layer grows to a desired thickness
#'
#' In the epitaxial layer growth process, suppose that the four experimental factors,
#' susceptor rotation method, nozzle position, deposition temperature, and deposition time (labeled A, B, Cand D)
#' are to be investigated at the two levels each.
#'
#' The purpose of this experiment is to find process conditions, that is, combinations of factor levels for A, B, C, and D,
#' under which the average thickness is close to the target 14.5 micrometre with variation as small as possible.
#' The most basic experimental design or plan is the full factorial design, which studies all possible combinations
#' of factors at two levels.
#'
#' @format A tibble with 16 observations, 4 factors (A,B,C,D), and three responses (ybar,s2,lns2)
#' \describe{
#' \item{Factor A}{Susceptor-rotation method. Low level is oscillating and high level is continuous}
#' \item{Factor B}{Nozzle position. Low level is 2 and high level is 6}
#' \item{Factor C}{Deposition temperature (Celsius). Low level is 1210 and high level is 1220}
#' \item{Factor D}{Deposition time. Low level is low and high level is high}
#' \item{ybar}{average thickness}
#' \item{s2}{variance of thickness}
#' \item{lns2}{log variance of thickness}
#' }
#' @source Wu, CF Jeff, and Michael S. Hamada. Experiments: planning, analysis, and optimization. John Wiley & Sons, 2011
"original_epitaxial"
