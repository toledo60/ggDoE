#' Obtain main effect plots in a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param ncols number of columns for facet grid. Default is 2
#' @param exclude_vars A vector containing variables to exclude
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @param showplot logical indicating to show the main effect plots. If false, a list of tibbles is returned used to obtain the main effects for each factor. Default is TRUE
#'
#' @return Main effects plots, or a list of tibble with calculated main effects for each factors if showplot=FALSE.
#' @export
#' @importFrom ggplot2 aes_ geom_point geom_line theme_bw labs facet_wrap scale_color_manual vars ylim element_blank
#' @importFrom dplyr group_by summarise "%>%" bind_rows filter
#' @importFrom utils stack

#'
#' @examples
#' main_effects(original_epitaxial,response='s2',exclude_vars = c('ybar','lns2'))
#' main_effects(original_epitaxial,response='ybar',exclude_vars=c('A','s2','lns2'),ncols=3)
main_effects <- function(design,response,ncols=2,
                         exclude_vars=c(),
                         color_palette = NA,
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

    vals = c()
    n = length(dat_list)

    for(i in 1:n){
      vals = c(vals,dat_list[[i]][[2]])
    }

    minval = min(vals)
    maxval = max(vals)

    dat = bind_rows(dat_list)

    dat[is.na(dat)] = 0

    vec_dat = as.vector(dat)

    melted_dat <- stack(vec_dat) %>%
      filter(values != 0) %>%
      filter(ind != response) %>%
      mutate(response_var = vec_dat[[response]]) %>%
      mutate(values = as.factor(values))

    factors_total = length(factor_names)

    if(is.na(color_palette)){
      factor_colors = rep("#21908CFF",factors_total)
    }
    else{
    factor_colors <- viridisPalette(factors_total,
                                    color_palette = color_palette,
                                    direction = direction,
                                    alpha = alpha)
    }

    p <- ggplot(melted_dat) +
      aes(x = values, y = response_var  , colour = ind, group=1) +
      geom_line(aes(group=1),size =0.5) +
      geom_point(size=1)+
      theme_bw() +
      ylim(minval,maxval)+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_color_manual(values = factor_colors)+
      facet_wrap(vars(ind),ncol = ncols)+
      labs(y=paste0("Mean of ",response),x='')

    return(p)
  }
}

