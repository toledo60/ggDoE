library(dplyr)

gg_boxplots <- function(data,response,factor,
                        point_size=0.75,
                        alpha=1,
                        color_palette = NA,
                        show_mean=TRUE,...){
  y <- enquo(response)
  factor <- enquo(factor)

  p <- ggplot(data, aes(x=!!factor,y=!!y,
                        fill = !!factor,
                        colour = !!factor)) +
    geom_boxplot(alpha = alpha,colour='black',
                 outlier.fill = "#ba3e30",
                 outlier.size = point_size) +
    guides(fill = 'none', colour = "none") +
    {if(show_mean)stat_summary(fun="mean", colour = "#ba3e30",size=0.75)}+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  return(p)
}

library(ggplot2)

gg_boxplots(data= throughput_dat,
            response = Throughput,
            factor=Machine,
            alpha=0.8,
            show_mean = FALSE)


