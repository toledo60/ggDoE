% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contour_plots.R
\name{contour_plots}
\alias{contour_plots}
\title{Contour plot(s) of a fitted linear model with ggplot2}
\usage{
contour_plots(rsm_contour, ncol = 2, stroke_size = 0.1, return_list = FALSE)
}
\arguments{
\item{rsm_contour}{Contour using an rsm/lm object}

\item{ncol}{Specify the number of column used in grid layout for contour plots. Default is ncol=2}

\item{stroke_size}{width of stroke relative to the size of the text. Ignored if less than zero.}

\item{return_list}{If TRUE, a list of the generated contour plots will be returned. Otherwise, the generated contour plots will be displayed in a grid layout. Default is FALSE}
}
\value{
contours plot(s) of a fitted linear model with ggplot2. If return_list is TRUE, then a list of the individual contour plot(s) will be returned
}
\description{
Contour plot(s) of a fitted linear model with ggplot2
}
\examples{
library(rsm)
heli = rsm::heli
heli.rsm <- rsm (ave ~ block + SO(x1, x2, x3, x4), data = heli)
rsm_contour <- contour (heli.rsm, ~x1+x2+x3, at = xs(heli.rsm),plot=FALSE)
contour_plots(rsm_contour,ncol=3)
}
