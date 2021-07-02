# Interaction Effects -----------------------------------------------------

#' Obtain interaction effects plot between two factors in a factorial design
#'
#' @param Design Data/Design used to plot
#' @param factr1 First factor
#' @param factr2 Second factor
#' @param response Response variable
#' @param point_size Change size of points
#' @param line_size Change thickness of lines
#' @param showplot  logical indicating to show main effect plot. If false, a tibble containing the data used to construct the plot is returned. Default is TRUE
#' @return data to calculate interaction effects, and interaction effects plot
#' @examples interaction_effect(Design = epitaxial, factr1 = A, factr2 = B, response = ybar)
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line theme_bw theme %+replace% element_rect
#' @importFrom dplyr enquo summarize group_by as_label
#' @export
interaction_effect <- function(Design,factr1,factr2,
                              response,point_size=1.5,
                              line_size=1,
                              showplot=TRUE){
  var1 = dplyr::enquo(factr1)
  var2 = dplyr::enquo(factr2)
  rsp = dplyr::enquo(response)

  dat <- summarize(group_by(Design, factor(!!var1),
                            factor(!!var2)), mean_response=mean(!!rsp),.groups = 'drop')
  names(dat) = c(as_label(var1),as_label(var2),"mean_response")

  if(showplot){
    plot <- ggplot(dat,aes_string(x=as_label(var1),y='mean_response',color=as_label(var2))) +
      geom_line(aes_string(group=as_label(var2)),
                size=line_size)+
      geom_point(size=point_size)+
      theme_bw() %+replace%
      theme(legend.background = element_rect(fill="gray96",
                                             size=0.5,
                                             linetype="solid"))+
      labs(y=paste0("Mean of ",as_label(rsp)))

    return(plot)
  }else{
    return(dat)
  }
}
