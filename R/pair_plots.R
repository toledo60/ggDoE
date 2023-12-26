#' Two Dimensional Projections of Latin Hypercube Designs
#'
#' @param design A Latin hypercube design. Can be matrix, or data.frame
#' @param point_color Change color of points in plots
#' @param grid Logial argument. Specify if a grid should be added to each projection.
#' The grid is constructed using seq(0,1,length=nrows(design)+1). Default is FALSE
#' @param point_size Change size of points in plots
#' @param n_columns number of columns for grid layout. Default is 2
#'
#' @importFrom ggplot2 aes geom_hline geom_vline labs geom_point
#' @importFrom graphics hist
#' @return A grid of scatter plots from all two dimensional projections of a Latin hypercube design.
#' @export
#'
#' @examples
#' set.seed(10)
#' X <- lhs::randomLHS(n=15,k=4)
#' pair_plots(X,n_columns = 3)
pair_plots <- function(design,
                       point_color="#21908CFF",
                       grid=FALSE,
                       point_size = 1.5,
                       n_columns=2){

  insight::check_if_installed('patchwork')
  dat <- as.data.frame(design)
  two_combns <- t(combn(ncol(dat),2))
  two_combns_names <- t(combn(colnames(dat),2))
  n_rows <- nrow(dat)

  iters <- nrow(two_combns)

  plot_list <- lapply(1:iters, function(i) {
    p1 <- ggplot(dat[, two_combns[i, ]],
                 aes(x = dat[, two_combns_names[i, 1]],
                     y = dat[, two_combns_names[i, 2]])) +
      geom_point(shape = "circle",
                 colour = point_color,
                 size = point_size) +
      labs(x = two_combns_names[i, 1],
           y = two_combns_names[i, 2])

    return(p1)
  })

  final_plot <- patchwork::wrap_plots(plot_list,
                                      ncol = n_columns) & theme_bw_nogrid() &
    {if(grid) geom_hline(yintercept = seq(0, 1, length = n_rows + 1),
                         color = 'grey',linetype = 2, alpha = 0.6)} &
    {if(grid) geom_vline(xintercept = seq(0, 1, length = n_rows + 1),
                         color = 'grey', linetype = 2, alpha = 0.6)}

  return(final_plot)
}



