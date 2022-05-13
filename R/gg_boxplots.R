#' Boxplots using ggplot2
#'
#' @param data provided dataset
#' @param response A character string indicating the response of the data
#' @param factor A character string indicating the factor of the data
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
#' gg_boxplots(data = throughput_dat,response = Throughput,factor = Machine)
#' gg_boxplots(data = throughput_dat,response = Throughput,factor = Machine,alpha = 0.7,
#' color_palette = 'viridis')
#' @importFrom dplyr enquo mutate select
#' @importFrom ggplot2 aes geom_boxplot guides stat_summary coord_flip scale_fill_manual
#' @importFrom ggplot2 theme_bw element_blank
gg_boxplots <- function(data,response,factor,
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
  factors_total = nrow(factor_levels)

  color_choice <- viridisPalette(factors_total,
                                  color_palette = color_palette,
                                  direction = direction,
                                  alpha = alpha)

  factor_colors_dat <- mutate(factor_levels, colors=color_choice)
  }

  p <- ggplot(data, aes(x=!!factor,y=!!y,
                        fill = !!factor)) +
    geom_boxplot(alpha = alpha,colour='black',
                 outlier.fill = "#ba3e30",
                 outlier.size = point_size) +
    guides(fill = 'none', colour = "none") +
    {if(!is.na(color_palette))scale_fill_manual(values = factor_colors_dat$colors)}+
    {if(show_mean)stat_summary(fun="mean", colour = "#ba3e30",size=0.75)}+
    {if(horizontal)coord_flip()}+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  return(p)
}
