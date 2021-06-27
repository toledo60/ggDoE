# Main Effects ------------------------------------------------------------

#' Obtain main effect plot for a factor in a factorial design
#'
#' @param Design Data/Design used to plot
#' @param factr Factor variable (without quotes)
#' @param response Response variable (without quotes)
#' @param colour A character string specifying the color of line/points
#' @param point_size Change size of points
#' @param line_size Change thickness of line
#' @param showplot  logical indicating to show main effect plot. If false, a tibble containing the data used to construct the plot is returned. Default is TRUE
#' @return main effect plot, or data used to construct main effect plot
#' @importFrom ggplot2 theme_bw as_label aes_string
#' @importFrom dplyr enquo summarize group_by
#' @examples
#' main_effect(Design = epitaxial, factr = B, response = ybar)
#' main_effect(Design = epitaxial, factr = B, response = s2, colour = "red", point_size = 4)
#' @export
main_effect <- function(Design,factr,response,
                       colour ="#4292c6",
                       point_size=1.5,line_size=1,
                       showplot=TRUE){

  var <- dplyr::enquo(factr)
  rsp <- dplyr::enquo(response)

  dat <- summarize(group_by(Design, factor(!!var)), mean_response=mean(!!rsp))
  names(dat) = c("var","mean_response")

  if(showplot){
    plot <- ggplot(dat,aes_string(x='var',y='mean_response',group=1)) +
      geom_line(aes(group=1),size=line_size,colour = colour)+
      geom_point(size=point_size,colour = colour)+
      theme_bw()+
      labs(y=paste0("Mean of ",as_label(rsp)),
           x=as_label(var))
    return(plot)
  }else{
    names(dat) = c(as_label(var),paste0("Mean_",as_label(rsp)))
    return(dat)
  }
}
