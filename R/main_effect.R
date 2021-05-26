# Main Effects ------------------------------------------------------------

#' Obtain main effect plot for a factor in our design with ggplot
#'
#' @param data Data/Design used to plot
#' @param factr Factor
#' @param response Response variable
#'
#' @return data to calculate main effect, and main effect plot
#' @importFrom ggplot2 theme_minimal
#' @examples
#' main_effect(data=epitaxial, factr = B, response = ybar)
#' @export
main_effect <- function(data,factr,response){
  var1 = dplyr::enquo(factr)
  response = dplyr::enquo(response)

  dat <- data %>%
    mutate(!!quo_name(var1) := factor(!!var1)) %>%
    group_by(!!var1) %>%
    summarise(y = mean(!!response),.groups = 'drop')

  plot <-  dat %>%
    ggplot(aes_(x=var1,y=~y,group=1)) +
    theme_minimal()+
    ylab(bquote("Mean of "*.(response)))+
    geom_line(aes(group=1),size=1,colour = "#4292c6")+
    geom_point(size=1.5,colour = "#4292c6")


  return(list(dat = dat,plot=plot))
}
