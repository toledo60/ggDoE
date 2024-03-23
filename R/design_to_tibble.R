#' Convert an object of class `design` to `tibble`
#'
#' @param design An object of class `design`
#' @param factors_to_numeric If TRUE, convert all factors to numeric type. The default ordering of levels is preserved when converting to numeric
#'
#' @return Converted design to tibble
#' @export
#'
#' @examples
#' dat <- DoE.base::fac.design(factor.names = list(temp = c(16,32),
#' time = c(4,12)),replications = 5, randomize = FALSE)
#' Thk <- c(116.1, 106.7, 116.5, 123.2, 116.9, 107.5, 115.5, 125.1, 112.6, 105.9,
#' 119.2, 124.5, 118.7, 107.1, 114.7, 124, 114.9, 106.5, 118.3, 124.7)
#' design <- DoE.base::add.response(dat, Thk)
#' design
#' design_to_tibble(design)
#' design_to_tibble(design, factors_to_numeric = TRUE)
design_to_tibble <- function(design,factors_to_numeric = FALSE){
  if(!inherits(design,'design')){
    stop("Design should be of class 'design'")
  }

  if(!factors_to_numeric){
    return(tibble::as_tibble(design))
  }

  as_double_factor <- function(x) {as.numeric(levels(x))[x]}

  design <- lapply(design, function(column) {
    if (is.factor(column)) {
      return(as_double_factor(column))
    } else {
      return(column)
    }
  })
  return(tibble::as_tibble(design))
}
