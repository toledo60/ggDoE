#' Boxplots using ggplot2
#'
#' @param data provided dataset
#' @param response Unquoted variable indicating the response of the data
#' @param factor Unquoted variable indicating the factor of the data
#' @param group_var Unquoted variable indicating the groups for facet_wrap
#' @param jitter_points Overlay jittered points to boxplots. Default is FALSE.
#' @param horizontal Determine whether to change the orientation of the plot. Default is FALSE
#' @param point_size Change size of points (outliers) in boxplots
#' @param alpha The alpha transparency, a number in [0,1]
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @param show_mean Dispaly the mean for each boxplot. Default is FALSE
#'
#' @return Boxplots created with ggplot2
#' @export
#'
#' @examples
#' data <- ToothGrowth
#' data$dose <- factor(data$dose,levels = c(0.5, 1, 2),labels = c("D0.5", "D1", "D2"))
#' gg_boxplots(data,response = len,factor = dose,alpha=0.6)
#' gg_boxplots(data,response = len,factor = dose,group_var = supp,
#' alpha=0.6,color_palette = 'viridis',jitter_points=TRUE)
#' @importFrom dplyr enquo mutate select
#' @importFrom ggplot2 aes geom_boxplot guides stat_summary coord_flip scale_color_manual geom_jitter position_jitterdodge
#' @importFrom ggplot2 theme_bw element_blank facet_wrap
gg_boxplots <- function(data,response,factor,
                        group_var = NULL,
                        jitter_points = FALSE,
                        horizontal = FALSE,
                        point_size=1,
                        alpha=1,
                        color_palette = NA,
                        direction=1,
                        show_mean=FALSE){
  y <- enquo(response)
  factor <- enquo(factor)

  if(!is.na(color_palette)){

  factor_levels <- unique(select(data,!!factor))
  factors_total <- nrow(factor_levels)

  color_choice <- viridisPalette(factors_total,
                                  color_palette = color_palette,
                                  direction = direction,
                                  alpha = alpha)

  factor_colors_dat <- mutate(factor_levels, colors=color_choice)
  }

  p <- ggplot(data, aes(x=!!factor,y=!!y,
                        color = !!factor)) +
    geom_boxplot(alpha = alpha,
                 outlier.shape = NA) +
    {if(jitter_points)geom_jitter(alpha=0.4,size=3,
                position=position_jitterdodge())}+
    guides(fill = 'none', colour = "none") +
    {if(!is.na(color_palette))scale_color_manual(values = factor_colors_dat$colors)}+
    {if(show_mean)stat_summary(fun="mean", colour = "#ba3e30",size=0.75)}+
    {if(horizontal)coord_flip()}+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if(missing(group_var)){
    return(p)
  }
  else{
    group_var <- enquo(group_var)
    data <- mutate(data,group_var = !!group_var)
    return(p+ facet_wrap(group_var))
  }
}
