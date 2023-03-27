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
#' @importFrom data.table data.table .SD
#' @examples
#' interaction_effects(adapted_epitaxial,response = 'ybar',exclude_vars = c('s2','lns2'))
interaction_effects <- function(design,response,
                                exclude_vars=c(),
                                linetypes = c('solid','dashed'),
                                colors = c("#4260c9" ,"#d6443c"),
                                n_columns=2,
                                showplot=TRUE){
  insight::check_if_installed('patchwork')

  design <- data.table(design)
  factor_names <- setdiff(names(design),c(response,exclude_vars))

  group_mean <- function(DT, response_var, group_by){
    return(DT[,.(mean_response=mean(.SD[[1]])),
              by= group_by, .SDcols = response_var
    ])
  }
  convert_to_factors <- function(DT, cols) {
    return(DT[,(cols) := lapply(.SD, 'as.factor'), .SDcols = cols])
  }
  interactions <- t(combn(factor_names,2))
  n_iteractions <- nrow(interactions)

  dat_list <- vector('list',length = n_iteractions)
  convert_to_factors(design,cols = factor_names)

  for (i in 1:n_iteractions) {
    dat_list[[i]] <- group_mean(DT=design,
                                group_by = c(interactions[i,1],
                                             interactions[i,2]),
                                response_var=response)
  }

  if(!showplot){
    return(dat_list)
  }
  else{
    vals <- c()

    for(i in 1:n_iteractions){
      vals <- c(vals,dat_list[[i]][[3]])
    }

    minval <- min(vals)
    maxval <- max(vals)

    plot_list <- vector('list',length = n_iteractions)
    for(i in 1:n_iteractions){
      plot_list[[i]] <- ggplot(dat_list[[i]],
                               aes(x = !!sym(interactions[i,1]),
                                   y = !!sym('mean_response'),
                                   group=!!sym(interactions[i,2]),
                                   colour=!!sym(interactions[i,2]),
                                   shape=!!sym(interactions[i,2]),
                                   linetype=!!sym(interactions[i,2])))+
        geom_line()+
        geom_point()+
        scale_linetype_manual(values=linetypes)+
        scale_color_manual(values=colors)+
        theme_bw()+
        ylim(minval,maxval)+
        theme(legend.background = element_rect(fill="gray96"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(y=paste0("Mean of ",response),
             x= interactions[i,1],
             colour=interactions[i,2],
             linetype=interactions[i,2],
             shape=interactions[i,2]
        )
    }
    return(patchwork::wrap_plots(plot_list,
                                 ncol = n_columns))
  }
}



