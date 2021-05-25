# Interaction Effects -----------------------------------------------------



#' Obtain interaction effects plot between two factors in our design with ggplot
#'
#' @param data
#' @param factr1
#' @param factr2
#' @param response
#'
#' @return data to calculate interaction effects, and interaction effects plot
#' @examples IA_effect(data = epitaxial, factr1 = A, factr2 = B, response = ybar)
#' @export
IA_effect <- function(data,factr1,factr2,response){
  var1 = dplyr::enquo(factr1)
  var2 = dplyr::enquo(factr2)
  response = enquo(response)

  dat <- data %>%
    mutate(!!quo_name(var1) := factor(!!var1), !!quo_name(var2) := factor(!!var2)) %>%
    group_by(!!var1,!!var2) %>%
    summarise(y = mean(!!response),.groups = 'drop')


  plot <- dat %>% ggplot(aes_(x=var1,y=~y,color=var2)) +
    geom_line(aes(group = {{var2}})) +
    geom_point(size=1.5)+
    theme_bw()+
    ylab(bquote("Mean of"*.(response)))+
    theme(legend.background = element_rect(fill="gray96", size=0.5, linetype="solid"))
  return(list(dat=dat,plot=plot))
}