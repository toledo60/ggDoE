#' Boxplots using ggplot2
#'
#' @param data provided dataset
#' @param x A character string indicating the factor of the data
#' @param y A character string indicating the response of the data
#' @param group_var A character string indicating the groups for facet_wrap
#' @param jitter_points Overlay jittered points to boxplots. Default is FALSE.
#' @param horizontal Determine whether to change the orientation of the plot. Default is FALSE
#' @param point_size Change size of points (outliers) in boxplots
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param ... additional parameters to be given to viridisPalette, such as alpha and direction
#' @param show_mean Display the mean for each boxplot. Default is FALSE
#'
#' @return Boxplots created with ggplot2
#' @export
#'
#' @examples
#' data <- ToothGrowth
#' data$dose <- factor(data$dose,levels = c(0.5, 1, 2),labels = c("D0.5", "D1", "D2"))
#' gg_boxplots(data,y= "len",x= "dose",alpha=0.6)
#' gg_boxplots(data,y = "len",x= "dose",group_var = "supp",
#' alpha=0.6,color_palette = 'viridis',jitter_points=TRUE)
#' @importFrom ggplot2 aes geom_boxplot guides stat_summary coord_flip sym
#' @importFrom ggplot2 facet_wrap scale_color_manual geom_jitter position_jitterdodge
gg_boxplots <- function(data,x,y,
                        group_var = NULL,
                        jitter_points = FALSE,
                        horizontal = FALSE,
                        point_size=1,
                        color_palette = NA,
                        show_mean=FALSE,
                        ...){

  dat <- data.frame(data)
  factor_vec <- dat[, x, drop = FALSE]

  if(!is.na(color_palette)){

    factor_levels <- unique(factor_vec)
    factors_total <- nrow(factor_levels)

    color_choice <- viridisPalette(factors_total,
                                   color_palette = color_palette,
                                   ...)

    factor_levels$colors <- color_choice
  }
  p <- ggplot(dat, aes(x=!!sym(x),y=!!sym(y),color = !!sym(x))) +
    geom_boxplot(outlier.shape = NA) +
    {if(jitter_points)geom_jitter(alpha=0.4,size=3,
                                  position=position_jitterdodge())}+
    guides(fill = 'none', colour = "none") +
    {if(!is.na(color_palette))scale_color_manual(values = factor_levels$colors)}+
    {if(show_mean)stat_summary(fun="mean", colour = "#ba3e30",size=0.75)}+
    {if(horizontal)coord_flip()}+
    theme_bw_nogrid()

  if(missing(group_var)){
    return(p)
  }
  else{
    dat[,group_var] <- factor(dat[,group_var])
    return(p+ facet_wrap(group_var))
  }
}
