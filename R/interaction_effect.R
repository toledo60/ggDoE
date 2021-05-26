# Interaction Effects -----------------------------------------------------



#' Obtain interaction effects plot between two factors in our design with ggplot
#'
#' @param data Data/Design used to plot
#' @param factr1 First factor
#' @param factr2 Second factor
#' @param response Response variable
#'
#' @return data to calculate interaction effects, and interaction effects plot
#' @examples interaction_effect(data = epitaxial, factr1 = A, factr2 = B, response = ybar)
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw aes_ theme %+replace% element_rect
#' @export
interaction_effect <- function(data,factr1,factr2,response){
  var1 = dplyr::enquo(factr1)
  var2 = dplyr::enquo(factr2)
  response = dplyr::enquo(response)

  dat <- data %>%
    mutate(!!quo_name(var1) := factor(!!var1), !!quo_name(var2) := factor(!!var2)) %>%
    group_by(!!var1,!!var2) %>%
    summarise(y = mean(!!response),.groups = 'drop')


  plot <- dat %>% ggplot(aes_(x=var1,y=~y,color=var2)) +
    geom_line(aes(group = {{var2}})) +
    geom_point(size=1.5)+
    theme_bw() %+replace%
    theme(legend.background = element_rect(fill="gray96",
                                           size=0.5,
                                           linetype="solid"))+
    ylab(bquote("Mean of"*.(response)))
  return(list(dat=dat,plot=plot))
}
