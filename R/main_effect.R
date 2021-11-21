# Main Effects ------------------------------------------------------------

#' Obtain main effect plot for a factor in a factorial design
#'
#' @param data Data/Design used to plot
#' @param factor_1 Factor variable (without quotes)
#' @param response_var Response variable (without quotes)
#' @param colour A character string specifying the color of line/points
#' @param point_size Change size of points
#' @param line_size Change thickness of line
#' @param linetype Change type of line. Default is 'solid'
#' @param shape Change shape of points. Default is 16
#'
#' @return main effects plot
#' @export
#'
#' @importFrom ggplot2 aes geom_point geom_line theme_bw labs
#' @importFrom poorman group_by summarise "%>%"
#' @examples main_effect(data = adapted_epitaxial, factor_1 = B, response_var = ybar)
main_effect  <- function(data, factor_1, response_var,
                         colour = "#4260c9",
                         point_size=1.5,line_size=0.8,
                         linetype = 'solid',
                         shape= 16) {
  factor1 <- substitute(factor_1)
  response <- substitute(response_var)

  x <- suppressWarnings(eval(bquote(
    data %>%
      group_by(.(factor1)) %>%
      summarise(mean_response = mean(.(response))) %>%
      ggplot(., aes(x = .(factor1), y = mean_response),group=1)+
      geom_line(aes(group=1),size=line_size,colour = colour,
                linetype = linetype)+
      geom_point(size=point_size,colour = colour,shape=shape)+
      theme_bw()+
      labs(y=paste0("Mean of ",response))
  )))

  return(x)
}
