#' Pareto Plot of Effects
#'
#' @param model Model of class "lm"
#' @param alpha specify the significance level to compute margin of errors. Numeric significance level, between 0 and 1. Default is alpha=0.05
#' @param showplot Default is TRUE, if false plot will not be shown and a tibble is returned with data used to create the pareto plot
#' @param margin_errors Default is TRUE, if false the cutoffs for margin of errors (ME) and simultaneous margin of error (SME) are not shown
#' @param method Character value. Method to calculate PSE. Default is Lenth. Options include: Zahn, WZahn, Lenth, RMS, Dong, JuanPena, Daniel. See Details.
#' @return A bar plot with ordered effects, margin of error (ME) and simultaneous margin of error (SME) cutoffs.
#' @importFrom stats reorder coef
#' @importFrom ggplot2 geom_hline geom_bar annotate labs coord_flip theme_classic aes_string scale_fill_discrete
#' @importFrom dplyr tibble
#' @importFrom unrepx PSE ME
#' @export
#'
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

#' @examples
#' m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
#' pareto_plot(m1)
#' pareto_plot(m1,method='Zahn',alpha=0.1)
#' pareto_plot(m1,margin_errors=FALSE)
pareto_plot <- function(model,alpha=0.05,method='Lenth',
                        margin_errors=TRUE,showplot=TRUE){
  if (inherits(model, "lm")) {
    i <- pmatch("(Intercept)", names(coef(model)))
    if (!is.na(i))
      effects <- coef(model)[-pmatch("(Intercept)", names(coef(model)))]
    obj <- 2 * effects
  }
  estimates <- obj
  PSE <- PSE(estimates,method = method)
  ME <- ME(estimates,method = method,alpha = alpha)[1]
  SME <- ME(estimates,method = method,alpha = alpha)[2]

  dat <- tibble("effect_names" = factor(names(estimates)),
                "effects" = estimates,
                'abs_effects' = abs(estimates),
                "cols" = ifelse(effects >0,'#d9a698','#9ecede'))

  sorted_dat <- dat[order(dat$abs_effects),]

  if(showplot){
    base_plot <- ggplot(sorted_dat,
                        aes_string(x = paste0("reorder(",'effect_names',", abs_effects)"),
                                   y = 'abs_effects',
                                   fill = 'cols')) +
      geom_bar(stat = "identity")+
      theme_classic()+
      coord_flip()+
      theme(legend.position = "top")+
      scale_fill_discrete(name = "Sign of Effect", labels = c("Negative", "Positive"))
    if(margin_errors){
      plot <- base_plot +
        geom_hline(yintercept = ME,linetype=2)+
        annotate("text",x=-Inf,y=ME,hjust=-0.2,vjust=-0.5,
                 label="ME",fontface="italic",size=2.8)+
        geom_hline(yintercept = SME,linetype=2)+
        annotate("text",x=-Inf,y=SME,hjust=-0.2,vjust=-0.5,
                 label="SME",fontface="italic",size=2.8)+
        labs(x="",y='absolute effects',
             caption = paste0(method," ME=",round(ME,2),","," SME=",round(SME,2)))
    }
    else{
      plot <- base_plot + labs(x="",y='absolute effects')
    }

    return(plot)
  }else{
    return(list(errors = tibble(alpha,PSE,ME,SME),
                dat = sorted_dat[,c(1,2,3)]))
  }
}
