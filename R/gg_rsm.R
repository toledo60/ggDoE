#' Contour plot(s) of a fitted linear model in ggplot2
#'
#' @param rsm_model Model of class "lm" or "glm"
#' @param form A formula, or a list of formulas
#' @param filled Determine if the surface plots should be filled by viridis color palette. Default is FALSE
#' @param decode This has an effect only if x is an rsm object or other model object that supports coded.data.
#' In such cases, if decode is TRUE, the coordinate axes are transformed to their decoded values.
#' @param n_columns number of columns for grid layout. Default is 2
#' @param stroke width of stroke relative to the size of the text. Ignored if less than zero. Only applied if contour plots are filled
#' @param size size of text for contour lines. Only applied if contour plots are filled
#' @param ... Other arguments passed on to contour(). For help with more arguments see ?rsm::contour.lm
#'
#' @return A grid of contour plot(s) of a fitted linear model in ggplot2
#' @export
#'
#' @importFrom graphics contour
#' @importFrom ggplot2 aes_string theme_bw theme_minimal labs element_blank element_text geom_contour geom_contour_filled
#' @importFrom gridExtra grid.arrange

#' @examples
#' heli = rsm::heli
#' heli.rsm <- rsm::rsm(ave ~ SO(x1, x2, x3, x4), data = heli)
#'
#' gg_rsm(heli.rsm,form = ~x1+x3+x4,at = rsm::xs(heli.rsm),n_columns=3)
#' gg_rsm(heli.rsm,form = ~x2+x3+x4,at = rsm::xs(heli.rsm),n_columns=3,filled = TRUE)
gg_rsm <- function(rsm_model,
                   form,
                   filled=FALSE,
                   decode=FALSE,
                   n_columns=2,
                   stroke = 0.15,
                   size=4,
                   ...){
  if(!inherits(rsm_model,'rsm')){
    stop("rsm_obj should be of class rsm")
  }

  rsm_contour <- contour(rsm_model, form, plot=FALSE,
                         decode=decode,
                         ...)

  xy_labels <- names(rsm_contour)
  splits <- strsplit(xy_labels,split = " ")
  x_lab <- unlist(lapply(splits, function(x) x[[1]]))
  y_lab <- unlist(lapply(splits, function(x) x[[3]]))


  xvec <- lapply(rsm_contour, function(x) x[[1]])
  yvec <- lapply(rsm_contour, function(x) x[[2]])
  zvec <- lapply(rsm_contour, function(x) x[[3]])
  zvec <-  lapply(zvec, c)

  df_list <- list()
  cplots <- list()

  if(!filled){
    for(i in 1:length(xvec)){
      df_list[[i]] <- cbind(expand.grid(xvec[[i]],yvec[[i]]),"z"=zvec[[i]])

      cplots[[i]] <- ggplot(df_list[[i]], aes_string(x='Var1', y='Var2')) +
        metR::geom_contour2(aes(z = z, label = ..level..))+
        theme_bw()+
        labs(caption=rsm_contour[[i]][[4]],
             x="",
             y=y_lab[i])+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.caption = element_text(hjust = 0.5)
        )
    }
  }
  else{
    for(i in 1:length(xvec)){
      df_list[[i]] <- cbind(expand.grid(xvec[[i]],yvec[[i]]),"z"=zvec[[i]])

      cplots[[i]] <- ggplot(df_list[[i]],
                            aes_string(x='Var1', y='Var2',z='z')) +
        geom_contour_filled()+
        geom_contour(color = "black", size = 0.1)+
        metR::geom_text_contour(aes_string(z = 'z'),
                                stroke = stroke,
                                size=size)+
        theme_minimal()+
        labs(caption=rsm_contour[[i]][[4]],
             x="",
             y=y_lab[i])+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.caption = element_text(hjust = 0.5),
              legend.position = 'none'
        )
    }
  }
  return(grid.arrange(grobs = cplots,
                      ncol=n_columns))
}
