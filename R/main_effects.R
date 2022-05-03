#' Obtain main effect plots in a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param ncols number of columns for facet grid. Default is 2
#' @param exclude_vars A vector containing variables to exclude
#' @param palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @param showplot logical indicating to show the main effect plots. If false, a list of tibbles is returned used to obtain the main effects for each factor. Default is TRUE
#'
#' @return Main effects plots, or a list of tibble with calculated main effects for each factors if showplot=FALSE.
#' @export
#' @importFrom ggplot2 aes_ geom_point geom_line theme_bw labs facet_wrap scale_color_manual vars
#' @importFrom dplyr group_by summarise "%>%" bind_rows
#'
#' @examples
#' main_effects(original_epitaxial,response='s2',exclude_vars = c('ybar','lns2'))
#' main_effects(original_epitaxial,response='ybar',exclude_vars=c('A','s2','lns2'),ncols=3)
main_effects <- function(design,response,ncols=2,
                         exclude_vars=c(),
                         palette = "viridis",
                         alpha=1,direction = 1,
                         showplot=TRUE){
  factor_names = setdiff(names(design),c(response,exclude_vars))
  dat_list <-  vector("list", length = length(factor_names))

  for (i in seq_along(factor_names)) {
    dat_list[[i]] = design %>%
      group_by(eval(parse(text=factor_names[i]))) %>%
      summarise(mean = mean(eval(parse(text=response))))
    colnames(dat_list[[i]]) = c(factor_names[i],response)
  }
  names(dat_list) = factor_names

  if(!showplot){
    return(dat_list)
  }
  else{
    dat = bind_rows(dat_list)
    melted_dat = reshape2::melt(dat,na.rm=TRUE,id.vars=response)

    factors_total = length(factor_names)

    factor_colors <- viridisPalette(factors_total,
                                    palette = palette,
                                    direction = direction,
                                    alpha = alpha)
    p <- ggplot(melted_dat) +
      aes_(x = ~value, y = as.name(response)  , colour = ~variable) +
      geom_line(size =0.5) +
      geom_point(size=1)+
      theme_bw() +
      theme(legend.position = "none") +
      scale_color_manual(values = factor_colors)+
      facet_wrap(vars(variable),ncol = ncols)+
      labs(y=paste0("Mean of ",response),x='')

    return(p)
  }
}
