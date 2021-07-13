#' Contour plot(s) of a fitted linear model with ggplot2
#'
#' @param rsm_contour Contour using an rsm/lm object
#' @param ncol Specify the number of column used in grid layout for contour plots. Default is ncol=2
#' @param return_list If TRUE, a list of the generated contour plots will be returned. Otherwise, the generated contour plots will be displayed in a grid layout. Default is FALSE
#' @param stroke_size width of stroke relative to the size of the text. Ignored if less than zero.
#' @importFrom metR geom_contour_fill geom_text_contour
#' @importFrom purrr map
#' @importFrom ggplot2 aes_string theme_bw labs element_blank element_text geom_contour
#' @importFrom gridExtra grid.arrange
#' @return contours plot(s) of a fitted linear model with ggplot2. If return_list is TRUE, then a list of the individual contour plot(s) will be returned
#' @export
#' @examples
#' library(rsm)
#' heli = rsm::heli
#' heli.rsm <- rsm (ave ~ block + SO(x1, x2, x3, x4), data = heli)
#' rsm_contour <- contour (heli.rsm, ~x1+x2+x3, at = xs(heli.rsm),plot=FALSE)
#' contour_plots(rsm_contour,ncol=3)
contour_plots <- function(rsm_contour,ncol=2,stroke_size=0.1,return_list =FALSE){
  xvec = map(rsm_contour,"x")
  yvec = map(rsm_contour,"y")
  zvec = map(rsm_contour,"z")
  zvec = lapply(zvec, c)


  xy_labels = names(rsm_contour)

  x_lab = unlist(map(strsplit(xy_labels, split = " "),1))
  y_lab = unlist(map(strsplit(xy_labels, split = " "),3))

  df_list = list()
  cplots = list()
  for(i in 1:length(xvec)){
    df_list[[i]]=cbind(expand.grid(xvec[[i]],yvec[[i]]),"z"=zvec[[i]])

    cplots[[i]] = ggplot(df_list[[i]], aes_string(x='Var1', y='Var2', z='z')) +
      metR::geom_contour_fill(show.legend = FALSE)+
      geom_contour(color = "white", size = 0.1)+
      metR::geom_text_contour(aes_string(z = 'z'),stroke = stroke_size)+
      theme_bw()+
      labs(x=x_lab[i],y=y_lab[i])+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.caption = element_text(hjust = 1,size=12)
      )
  }
  names(cplots) = xy_labels
  if(return_list){
    return(cplots)
  }
  else{
    return(grid.arrange(grobs = cplots,ncol=ncol))
  }
}
