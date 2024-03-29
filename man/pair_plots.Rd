% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pair_plots.R
\name{pair_plots}
\alias{pair_plots}
\title{Two Dimensional Projections of Latin Hypercube Designs}
\usage{
pair_plots(
  design,
  point_color = "#21908CFF",
  point_size = 1.5,
  grid = c(-1, -1),
  n_columns = 2
)
}
\arguments{
\item{design}{A Latin hypercube design. Can be matrix, or data.frame}

\item{point_color}{Change color of points in plots}

\item{point_size}{Change size of points in plots}

\item{grid}{A numeric vector specifying the dimensions of the grid to overlay.}

\item{n_columns}{number of columns for grid layout. Default is 2}
}
\value{
A grid of scatter plots from all two dimensional projections of a Latin hypercube design.
}
\description{
Two Dimensional Projections of Latin Hypercube Designs
}
\examples{
set.seed(10)
X <- lhs::randomLHS(n=12,k=4)
pair_plots(X,n_columns = 3)
pair_plots(X,n_columns = 3, grid = c(3,2))
}
