#' Box-Cox Transformations
#'
#' @param model Model used for Box-Cox transformation
#' @param lambda sequence of lambda values to consider for plot. Default is seq(-2,2,1/10)
#' @param showlambda Default is TRUE, show lambda values in plot
#' @param lambdaSF Digits to round lambda values shown in plot
#' @param showplot Default is TRUE, if false plot will not be shown and a tibble is returned with a 95\% confidence interval for lambda and lambda value which maximizes log-likelihood
#'
#' @importFrom ggplot2 geom_segment geom_vline geom_hline element_blank geom_text
#' @importFrom stats qchisq
#' @importFrom dplyr tibble
#' @return Box-Cox transformation plot with 95\% confidence interval of lambda values to consider
#' @export
#'
#' @examples
#' model <- lm(s2 ~ (A+B+C+D),data = adapted_epitaxial)
#' boxcox_transform(model,lambda = seq(-5,5,0.2))
#' boxcox_transform(model,lambda = seq(-5,5,0.2),showplot=FALSE)
boxcox_transform <- function(model,lambda= seq(-2,2,1/10),
                             showlambda = TRUE, lambdaSF = 3,showplot=TRUE){

  boxcox_object <- MASS::boxcox(model, plotit = FALSE,interp=TRUE,lambda=lambda)

  # create new dataframe to hold all x and y points
  x <- unlist(boxcox_object$x)
  y <- unlist(boxcox_object$y)

  # compute start and end of each line segment
  xstart <- x[-1]
  ystart <- y[-1]
  xend <- x[-(length(x))]
  yend <- y[-(length(y))]
  boxcox_unlist <- data.frame(xstart, ystart, xend, yend)

  # obtain best lambda
  best_lambda <- x[which.max(y)]
  rounded_lambda <- round(best_lambda, lambdaSF)
  min_y <- min(y)

  # compute accepted range of lambda transformation
  accept_inds <- which(y > max(y) - 1/2 * qchisq(.95,1))
  accept_range <- x[accept_inds]
  conf_lo <- round(min(accept_range), lambdaSF)
  conf_hi <- round(max(accept_range), lambdaSF)

  if(showplot){
    plot <- ggplot(data = boxcox_unlist) +
      geom_segment(aes(x = xstart, y = ystart, xend = xend, yend = yend),
                   size=1.3,color="#21908CFF") +
      labs(x = "Lambda", y = "Log-likelihood",title = "Boxcox Transformation",
           subtitle = paste0("95% CI for Lambda:",' (',conf_lo,', ',conf_hi,')')) +
      geom_vline(xintercept = best_lambda, linetype = "dotted",
                 color = '#2dab03',size=1.1) +
      geom_vline(xintercept = conf_lo, linetype = "dotted",
                 color = "indianred3",size=1.1) +
      geom_vline(xintercept = conf_hi, linetype = "dotted",
                 color = "indianred3",size=1.1) +
      geom_hline(yintercept = y[min(accept_inds)],
                 linetype = "dashed")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


    # add label if show lambda range
    if (showlambda) {
      return(plot +
               geom_text(aes(x = best_lambda-0.1,
                             label = as.character(rounded_lambda), y = min_y),color='#2dab03') +
               geom_text(aes(x = conf_lo-0.1,
                             label = as.character(conf_lo), y = min_y), color = "indianred3") +
               geom_text(aes(x = conf_hi-0.1,
                             label = as.character(conf_hi), y = min_y), color = "indianred3"))
    }else {
      return (plot)
    }
  }else{
    return(tibble("best_lambda" = best_lambda,
                  "lambda_low"=conf_lo,
                  "lambda_high"=conf_hi))
  }
}
