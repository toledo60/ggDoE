#' Two-Factor interaction effects plot for a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param exclude_vars A vector containing variables to exclude
#' @param linetypes Change linetypes. Default are ('solid','dashed)
#' @param colors Change color of lines/points. Default are ("#4260c9" ,"#d6443c")
#' @param ncols number of columns for facet grid. Default is 2
#' @param showplot logical indicating to show the interaction effect plots. If false, a list of tibbles is returned used to obtain the interaction effects for each factor. Default is TRUE
#'
#' @return interaction effects plot between two factors
#' @export
#'
#' @importFrom ggplot2 aes geom_line geom_point theme labs element_rect scale_linetype_manual
#' @importFrom ggplot2 scale_color_manual theme_bw ylim element_blank
#' @importFrom dplyr mutate_at setdiff group_by summarise "%>%"
#' @importFrom utils combn
#' @importFrom gridExtra grid.arrange

#' @examples
#' interaction_effects(adapted_epitaxial,response = 'ybar',exclude_vars = c('s2','lns2'))

interaction_effects <- function(design,response,
                                exclude_vars=c(),
                                linetypes = c('solid','dashed'),
                                colors = c("#4260c9" ,"#d6443c"),
                                ncols=2,
                                showplot=TRUE)
{

  factor_names <- setdiff(names(design),c(response,exclude_vars))

  interactions <- t(combn(factor_names,2))
  dat_list <- vector('list',length = nrow(interactions))

  dat <- design %>% mutate_at(factor_names,factor)

  for(i in 1:nrow(interactions)){
    dat_list[[i]] <- dat %>%
      group_by(eval(parse(text=interactions[i,1])),
               eval(parse(text=interactions[i,2]))) %>%
      summarise(mean= mean(eval(parse(text=response))),.groups='drop')
    colnames(dat_list[[i]]) = c('factor1','factor2','response','group')
  }


  if(showplot){

    vals <- c()
    n <- length(dat_list)

    for(i in 1:n){
      vals <- c(vals,dat_list[[i]][[3]])
    }

    minval <- min(vals)
    maxval <- max(vals)

    plot_list <- vector('list',length = n)

    for(i in 1:n){

      plot_list[[i]] <- dat_list[[i]] %>%
        ggplot(., aes(x = factor1, y = response,group=factor2,
                      colour=factor2,
                      shape=factor2,
                      linetype=factor2) )+
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
    return(grid.arrange(grobs=plot_list,ncol=ncols))
  }
  else{
    return(dat_list)
  }
}
