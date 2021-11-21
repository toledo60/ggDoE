# Interaction Effects -----------------------------------------------------

#' Obtain interaction effects plot between two factors in a factorial design
#'
#' @param data Data/Design used to plot
#' @param factor_1 First factor
#' @param factor_2 Second factor
#' @param response_var Response variable
#' @param linetypes Change linetypes. Default are ('solid','dashed)
#' @param colors Change color of lines/points. Default are ("#4260c9" ,"#d6443c")
#' @param ... additional customization for both geom_line and geom_point (optional)
#'
#' @return interaction effects plot
#' @export
#'
#' @examples
#' interaction_effect(data = adapted_epitaxial, factor_1 = A,
#' factor_2 = B, response_var = ybar)
#' @importFrom ggplot2 aes geom_line geom_point theme labs element_rect scale_linetype_manual
#' @importFrom ggplot2 scale_color_manual theme_bw
#' @importFrom poorman group_by mutate summarise "%>%"
interaction_effect  <- function(data, factor_1,
                                factor_2,
                                response_var,
                                linetypes = c('solid','dashed'),
                                colors = c("#4260c9" ,"#d6443c"),...) {

  factor1 <- substitute(factor_1)
  factor2 <- substitute(factor_2)
  response <- substitute(response_var)

  x <- suppressWarnings((eval(bquote(
    data %>%
      group_by(.(factor1),.(factor2),.add=TRUE) %>%
      summarise(mean_response = mean(.(response)),.groups = "drop_last") %>%
      mutate(factor1 = as.factor(.(factor1) ) ) %>%
      mutate(factor2 = as.factor(.(factor2) ) ) %>%
      ggplot(., aes(x = (factor1), y = mean_response,group=factor2,
                    colour=factor2,
                    shape=factor2,
                    linetype=factor2) )+
      geom_line(...)+
      geom_point(...)+
      scale_linetype_manual(values=linetypes)+
      scale_color_manual(values=colors)+
      theme_bw()+
      theme(legend.background = element_rect(fill="gray96"))+
      labs(y=paste0("Mean of ",response),
           x= factor1,
           colour=factor2,
           linetype=factor2,
           shape=factor2
      )
  ))))

  return(x)
}
