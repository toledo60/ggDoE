#' Two-Factor interaction effects plot for a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param exclude_vars A vector containing variables to exclude
#' @param linetypes Change linetypes. Default are ('solid','dashed)
#' @param colors Change color of lines/points. Default are ("#4260c9" ,"#d6443c")
#' @param n_columns number of columns for facet grid. Default is 2
#' @param showplot logical indicating to show the interaction effect plots. If false, a list of tibbles is returned used to obtain the interaction effects for each factor. Default is TRUE
#'
#' @return interaction effects plot between two factors
#' @export
#'
#' @importFrom ggplot2 aes geom_line geom_point theme labs element_rect scale_linetype_manual sym
#' @importFrom ggplot2 scale_color_manual theme_bw ylim element_blank
#' @importFrom utils combn
#' @importFrom stats aggregate
#' @importFrom patchwork wrap_plots

#' @examples
#' interaction_effects(adapted_epitaxial,response = 'ybar',exclude_vars = c('s2','lns2'))
interaction_effects <- function(design,response,
                                exclude_vars = c(),
                                linetypes = c('solid','dashed'),
                                colors = c("#4260c9" ,"#d6443c"),
                                n_columns = 2,
                                showplot = TRUE){
  if(length(colors) != 2 & !inherits(colors,'character') ){
    stop('colors must a character vector of length 2')
  }
  if(length(linetypes) != 2 & !inherits(linetypes,'character') ){
    stop('linetypes must a character vector of length 2')
  }
  insight::check_if_installed('patchwork')

  if(inherits(design,'design')){
    design <- design_to_tibble(design,factors_to_numeric = TRUE)
  }

  factor_names <- setdiff(names(design),c(response,exclude_vars,'Blocks'))
  design[,factor_names] <- lapply(design[,factor_names], as.factor)

  interactions <- t(combn(factor_names,2))
  n_iteractions <- nrow(interactions)

  dat_list <- lapply(seq_len(n_iteractions), function(i) {
    aggregated_data <- aggregate(design[response],
                                 by = list(design[[interactions[i, 1]]],
                                           design[[interactions[i, 2]]]),
                                 FUN = mean)
    colnames(aggregated_data) <- c(interactions[i, 1], interactions[i, 2], 'mean_response')
    return(aggregated_data)
  })

  if(!showplot){
    return(dat_list)
  }
  else{
    vals <- sapply(dat_list, function(df) df[[3]])
    minval <- min(vals, na.rm = TRUE)
    maxval <- max(vals, na.rm = TRUE)

    plot_list <- lapply(seq_len(n_iteractions), function(i) {
      ggplot(dat_list[[i]],
             aes(x = !!sym(interactions[i, 1]),
                 y = !!sym('mean_response'),
                 group = !!sym(interactions[i, 2]),
                 colour = !!sym(interactions[i, 2]),
                 shape = !!sym(interactions[i, 2]),
                 linetype = !!sym(interactions[i, 2]))) +
        geom_line() +
        geom_point() +
        scale_linetype_manual(values = linetypes) +
        scale_color_manual(values = colors) +
        ylim(minval, maxval) +
        labs(y = paste0("Mean of ", response),
             x = interactions[i, 1],
             colour = interactions[i, 2],
             linetype = interactions[i, 2],
             shape = interactions[i, 2])
    })

    if(length(plot_list) == 1){
      n_columns <- 1
    }
    final_plot <- wrap_plots(plot_list,ncol = n_columns) &
      theme_bw() &
      theme(legend.background = element_rect(fill="gray96"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    return(final_plot)
  }
}
