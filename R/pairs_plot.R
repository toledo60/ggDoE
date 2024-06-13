#' Two Dimensional Projections of Latin Hypercube Designs
#'
#' @param design A Latin hypercube design. Can be matrix, or data.frame
#' @param point_color Change color of points in plots
#' @param grid A numeric vector specifying the dimensions of the grid to overlay.
#' @param point_size Change size of points in plots
#' @param n_columns number of columns for grid layout. Default is 2
#'
#' @importFrom ggplot2 aes geom_hline geom_vline labs geom_point
#' @importFrom graphics hist
#' @importFrom patchwork wrap_plots
#' @return A grid of scatter plots from all two dimensional projections of a Latin hypercube design.
#' @export
#'
#' @examples
#' set.seed(10)
#' X <- lhs::randomLHS(n=12,k=4)
#' pairs_plot(X,n_columns = 3)
#' pairs_plot(X,n_columns = 3, grid = c(3,2))
pairs_plot <- function(design,
                       point_color="#21908CFF",
                       point_size = 1.5,
                       grid = c(-1,-1),
                       n_columns=2){

  dat <- as.data.frame(design)
  two_combns <- t(combn(ncol(dat),2))
  two_combns_names <- t(combn(colnames(dat),2))

  iters <- nrow(two_combns)
  lower <- function(x) {
    min_val <- min(x, na.rm = TRUE)
    if (min_val == 0 || min_val == 1) {
      return(min_val - 0.1)
    } else {
      return(floor(min_val) - 0.05 * abs(floor(min_val)))
    }
  }

  upper <- function(x) {
    max_val <- max(x, na.rm = TRUE)
    if (max_val == 0 || max_val == 1) {
      return(max_val + 0.1)
    } else {
      return(ceiling(max_val) + 0.05 * abs(ceiling(max_val)))
    }
  }

  plot_list <- lapply(1:iters, function(i) {
    x_data <- dat[[two_combns_names[i, 1]]]
    y_data <- dat[[two_combns_names[i, 2]]]

    x_lower <- lower(x_data)
    x_upper <- upper(x_data)
    y_lower <- lower(y_data)
    y_upper <- upper(y_data)

    p1 <- ggplot(dat[, two_combns[i, ]],
                 aes(x = dat[, two_combns_names[i, 1]],
                     y = dat[, two_combns_names[i, 2]])) +
      geom_point(shape = "circle",
                 colour = point_color,
                 size = point_size) +
      geom_hline(yintercept = seq(y_lower, y_upper, length.out = grid[2] + 1),
                 color = 'grey', linetype = 2, alpha = 0.7) +
      geom_vline(xintercept = seq(x_lower, x_upper, length.out = grid[1] + 1),
                 color = 'grey', linetype = 2, alpha = 0.7)+
      labs(x = two_combns_names[i, 1],
           y = two_combns_names[i, 2])

    return(p1)
  })

  if(iters == 1){
    n_columns <- 1
  }
  final_plot <- wrap_plots(plot_list,ncol = n_columns) & theme_bw_nogrid()
  return(final_plot)
}
