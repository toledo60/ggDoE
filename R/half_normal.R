#' Half-Normal Effects Plots
#'
#' @param model object of class "lm"
#' @param method Character value. Method to calculate PSE. Default is Lenth. Options include: Zahn, WZahn, Lenth, RMS, Dong, JuanPena, Daniel. See Details.
#' @param alpha specify the significance level to compute margin of errors. Numeric significance level, between 0 and 1. Default is alpha=0.05
#' @param label_active If TRUE, active effects are labeled if the effects cross the computed margin of error (ME). See method argument for more details
#' @param ref_line Dafault is TRUE, if FALSE the abline with slope (1/PSE) is not displayed. Reference line should follow along most points that are not considered outliers.
#' @param margin_errors Default is FALSE, if TRUE the cutoffs for margin of errors (ME) and simultaneous margin of error (SME) are shown
#' @param point_color Change color of points in plot
#' @param showplot Default is TRUE, if FALSE plot will not be shown and a tibble is returned used to create the plot along with the calculated PSE,ME,SME
#' @return A tibble with the absolute effects and half-normal quantiles. A ggplot2 version of halfnormal plot for factorial effects is returned
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs geom_vline annotate geom_abline element_blank
#' @importFrom stats qnorm coef
#' @importFrom utils tail
#' @export
#'
#' @details
#' The method argument is a simple wrapper for the function PSE() from the unrepx R package.
#' For more details you can use ?unrepx::PSE(). The \emph{method} arguement implements methods of estimating the standard error of
#' effects estimates from unreplicatd designs. The methods include
#' \itemize{
#' \item{Daniel: }{The 68.3rd quantile of the absolute effects. See Daniel (1959) }
#' \item{Dong: }{The RMS method, applied after excluding all
#' effects that exceed 2.5 * PSE(effects, "SMedian") in absolute value. See Dong (1993)}
#' \item{JuanPena: }{An iterated median method whereby we repeatedly calculate the
#' median of the absolute effects that don't exceed 3.5 times the previous median, until it stabilizes.
#' The estimate is the final median, divided by .6578. See Juan and Pena (1992).}
#' \item{Lenth (Default): }{The SMedian method, applied after excluding all effects that exceed 2.5 * PSE(effects, "SMedian")
#' in absolute value. See Lenth (1989)}
#' \item{RMS: }{Square root of the mean of the squared effects. This is not a good PSE in the presence of active effects,
#' but it is provided for sake of comparisons}
#' \item{SMedian: }{1.5 times the median of the absolute effects}
#' \item{Zahn, WZahn: }{The Zahn method is the slope of the least-squares line fitted to the first m points of unrepx::hnplot(effects, horiz = FALSE),
#' where m = floor(.683 * length(effects)). (This line is fitted through the origin.)
#' The WZahn method is an experimental version of Zahn's method,
#' based on weighted least-squares
#' with weights decreasing linearly from m - .5 to .5, but bounded above by .65m}
#'
#' }
#'
#' @references
#'
#' Daniel, C (1959) Use of Half-Normal Plots in Interpreting Factorial Two-Level Experiments. Technometrics, 1(4), 311-341 \cr \cr

#' Dong, F (1993) On the Identification of Active Contrasts in Unreplicated Fractional Factorials. Statistica Sinica 3, 209-217 \cr \cr

#' Hamada and Balakrishnan (1998) Analyzing Unreplicated Factorial Experiments: A Review With Some New Proposals. Statistica Sinica 8, 1-41 \cr \cr

#' Juan, J and Pena, D (1992) A Simple Method to Identify Significant Effects in Unreplicated Two-Level Factorial Designs. Communications in Statistics: Theory and Methods 21, 1383-1403 \cr \cr

#' Lenth, R (1989) Quick and Easy Analysis of Unrelicated Factorials Technometrics 31(4), 469-473 \cr \cr

#' Zahn, D (1975) Modifications of and Revised Critical Values for the Half-Normal Plot. Technometrics 17(2), 189-200

#' @examples m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
#' half_normal(m1)
#' half_normal(m1,alpha=0.1,label_active=TRUE,margin_errors=TRUE)
#' half_normal(m1,method='Zahn',alpha=0.1,ref_line=TRUE,
#'             label_active=TRUE,margin_errors=TRUE)
half_normal <- function(model,method='Lenth',
                        alpha=0.05,
                        label_active=FALSE,
                        ref_line = FALSE,
                        margin_errors = FALSE,
                        point_color="#21908CFF",
                        showplot=TRUE){
  if (inherits(model, "lm")) {
    i <- pmatch("(Intercept)", names(coef(model)))
    if (!is.na(i))
      effects <- coef(model)[-pmatch("(Intercept)", names(coef(model)))]
    estimates <- 2 * effects
  }
  insight::check_if_installed(c('unrepx','ggrepel'))

  PSE <- unrepx::PSE(estimates,method = method)
  ME <- unrepx::ME(estimates,method = method,alpha = alpha)[1]
  SME <- unrepx::ME(estimates,method = method,alpha=alpha)[2]

  effs <- sort(abs(estimates))
  names <- names(effs)
  m <- length(estimates)
  r <- c(1:m)
  zscore <- c(rep(0,m))

  for (i in 1:m) {
    zscore[i] <- qnorm( ( ( r[i]-.5)/m+1)/2 )
    if(label_active){
      logc <- (abs(effs[i])<= ME)
      if (logc) {names[i]<-NA}}
  }

  dat <- tibble::tibble("effects"=names,
                        "absolute_effects"=effs,
                        "half_normal_quantiles"=zscore)

  if(showplot){
    base_plot <- ggplot(dat,aes_string(x= 'absolute_effects',
                                       y = 'half_normal_quantiles',
                                       label='effects')) +
      geom_point(color = point_color, size = 2.5)+
      ggrepel::geom_text_repel(data = dat,
                               min.segment.length = Inf,
                               nudge_y = 0.001*max(dat$half_normal_quantiles),
                               nudge_x = 0.001*max(dat$absolute_effects),
                               na.rm = TRUE
      )+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      labs(x="absolute effects",y="half-normal quantiles")

    if(ref_line & margin_errors){
      plt <- base_plot + geom_abline(intercept=0,slope = 1/PSE,linetype=2)+
        geom_vline(xintercept = ME,linetype=3)+
        annotate("text",x=ME,y=-Inf,hjust=-0.2,vjust=-0.5,
                 label="ME",fontface="italic",size=2.8)+
        geom_vline(xintercept=SME,linetype=3)+
        annotate("text",x=SME,y=-Inf,hjust=-0.2,vjust=-0.5,
                 label="SME",fontface="italic",size=2.8)+
        labs(x="absolute effects",y="half-normal quantiles",
             caption = paste0(method," ME=",round(ME,2),",",
                              " SME=",round(SME,2)))
    }
    else if (!ref_line & !margin_errors){
      plt <- base_plot
    }
    else if(!ref_line & margin_errors){
      plt <- base_plot +geom_vline(xintercept = ME,linetype=3)+
        annotate("text",x=ME,y=-Inf,hjust=-0.2,vjust=-0.5,
                 label="ME",fontface="italic",size=2.8)+
        geom_vline(xintercept=SME,linetype=3)+
        annotate("text",x=SME,y=-Inf,hjust=-0.2,vjust=-0.5,
                 label="SME",fontface="italic",size=2.8)+
        labs(x="absolute effects",y="half-normal quantiles",
             caption = paste0(method," ME=",round(ME,2),",",
                              " SME=",round(SME,2)))
    }
    else if(ref_line & !margin_errors){
      plt <- base_plot + geom_abline(intercept=0,slope = 1/PSE,linetype=2)
    }
    return(plt)
  }
  else{
    return(dat)
  }
}
