#' Contour plot(s) of a fitted linear model in ggplot2
#'
#' @param rsm_model Model of class "rsm"
#' @param formula A formula, or a list of formulas
#' @param decode This has an effect only if x is an rsm object or other model object that supports coded.data.
#' In such cases, if decode is TRUE, the coordinate axes are transformed to their decoded values.
#' @param n_columns number of columns for grid layout. Default is 2
#' @param text_size size of text for labelled contour lines. Default is 3
#' @param bins Number of contour bins. Overridden by binwidth
#' @param ... Other arguments passed on to contour(). For help with more arguments see ?rsm::contour.lm

#' @return A grid of contour plot(s) of a fitted linear model in 'ggplot2'
#' @export
#'
#' @importFrom graphics contour
#' @importFrom ggplot2 aes theme_bw theme_minimal labs element_blank element_text geom_contour_filled
#' @importFrom patchwork wrap_plots

#' @examples
#' \dontrun{
#' heli.rsm <- rsm::rsm(ave ~ SO(x1, x2, x3),data = rsm::heli)
#'
#' gg_rsm(heli.rsm,formula = ~x1+x2+x3,at = rsm::xs(heli.rsm),n_columns=3)
#'
#'}
gg_rsm <- function(rsm_model,
                   formula,
                   decode = FALSE,
                   n_columns = 2,
                   text_size = 3,
                   bins=6,
                   ...){
  if(!inherits(rsm_model,'rsm')){
    stop("rsm_obj should be of class rsm")
  }
  insight::check_if_installed('geomtextpath')

  rsm_contour <- contour(rsm_model, formula, plot=FALSE,
                         decode=decode,...)

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

  round_labs <- function(x) {
    pat <- "(-)?[[:digit:]]+\\.[[:digit:]]*"
    m <- gregexpr(pat, x)
    regmatches(x,m) <- lapply(regmatches(x,m), function(X) round(as.numeric(X),3))
    return(x)
  }

  for(i in seq_along(xvec)){
    df_list[[i]] <- cbind(expand.grid(xvec[[i]],yvec[[i]]),"z"=zvec[[i]])

    cplots[[i]] <- ggplot(df_list[[i]],
                          aes(x=!!sym('Var1'), y=!!sym('Var2'),z=!!sym('z'))) +
      geom_contour_filled(bins=bins)+
      geomtextpath::geom_textcontour(bins=bins,size=text_size) +
      theme_minimal()+
      labs(caption=round_labs(rsm_contour[[i]][[4]]),
           x="",
           y=y_lab[i])+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.caption = element_text(hjust = 0.5),
            legend.position = 'none')
  }
  return(wrap_plots(cplots, ncol = n_columns))
}






