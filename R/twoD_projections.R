#' Two Dimensional Projections of Latin Hypercube Designs
#'
#' @param design A Latin hypercube design. Can be matrix, or data.frame
#' @param point_color Change color of points in plots
#' @param grid Logial argument. Specify if a grid should be added to each projection.
#' The grid is constructed using seq(0,1,length=nrows(design)+1). Default is FALSE
#' @param point_size Change size of points in plots
#' @param ncols number of columns for grid layout. Default is 2
#'
#' @importFrom ggplot2 xlim ylim aes theme_bw element_blank geom_hline geom_vline labs geom_point
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics hist
#' @return A grid of scatter plots from all two dimensional projections of a Latin hypercube design.
#' @export
#'
#' @examples
#' set.seed(100)
#' X <- lhs::randomLHS(n = 10, k = 4)
#' twoD_projections(X,ncols=3,grid = TRUE)
twoD_projections <- function(design,
                             point_color="#21908CFF",
                             grid=FALSE,
                             point_size = 1.5,
                             ncols=2){

  check_LHD <- function(design)
  {
    # This function was taken from the following stackexchange question:
    # R Carnell (https://stats.stackexchange.com/users/212798/r-carnell),
    # Is there a way to check if sample obeys the Latin Hypercube Sampling rule?,
    # URL (version: 2020-05-10): https://stats.stackexchange.com/q/465655

    if (any(apply(design, 2, min) <= 0))
      return(FALSE)
    if (any(apply(design, 2, max) >= 1))
      return(FALSE)
    if (any(is.na(design)))
      return(FALSE)
    # check that the matrix is a latin hypercube
    g <- function(Y)
    {
      # check that this column contains all the cells
      breakpoints <- seq(0, 1, length = length(Y) + 1)
      h <- hist(Y, plot = FALSE, breaks = breakpoints)
      all(h$counts == 1)
    }
    # check all the columns
    return(all(apply(design, 2, g)))
  }

  if(!check_LHD(design)){
    stop('Make sure design follows Latin Hypercube Sampling conditions: \n
         Matrix must be a latin hypercube with values between (0,1)')
  }
  else{

    dat <- as.data.frame(design)
    two_combns <- t(combn(ncol(dat),2))
    two_combns_names <-t(combn(colnames(dat),2))
    n_rows <- nrow(dat)

    iters <- nrow(two_combns)
    plot_list <- vector("list", iters)

    if(grid){
      for(i in 1:iters) {

        plot_list[[i]] <- local({
          i <- i
          p1 <-ggplot(dat[,two_combns[i,]],
                      aes(x = dat[,two_combns_names[i,1]],
                          y = dat[,two_combns_names[i,2]])) +
            geom_point(shape = "circle",
                       colour = point_color,
                       size = point_size) +
            geom_hline(yintercept = seq(0,1,length=n_rows+1),
                       color='grey',linetype=2,alpha=0.6)+
            geom_vline(xintercept = seq(0,1,length=n_rows+1),
                       color='grey',linetype=2,alpha=0.6)+
            labs(x=two_combns_names[i,1],
                 y=two_combns_names[i,2])+
            xlim(c(0,1))+
            ylim(c(0,1))+
            theme_bw()+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

        })
      }

    }
    else{
      for(i in 1:iters) {
        plot_list[[i]] <- local({
          i <- i
          p1 <-ggplot(dat[,two_combns[i,]],
                      aes(x = dat[,two_combns_names[i,1]],
                          y = dat[,two_combns_names[i,2]])) +
            geom_point(shape = "circle",
                       colour = point_color,
                       size = point_size) +
            labs(x=two_combns_names[i,1],
                 y=two_combns_names[i,2])+
            xlim(c(0,1))+
            ylim(c(0,1))+
            theme_bw()+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

        })
      }

    }
    return(grid.arrange(grobs=plot_list,
                        ncol=ncols))
  }

}
