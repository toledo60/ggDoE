#' Lenth’s method: (DEPRECATED use pareto_plot() instead)
#'
#' @param mod A linear model
#' @param alpha specify the significance level. Default is alpha=0.05
#' @param method Character value. Method to calculate PSE. Default is Lenth. Options include: Zahn, WZahn, Lenth, RMS, Dong, JuanPena, Daniel
#' @param showplot Default is TRUE, if false plot will not be shown and a tibble is returned used to create the plot
#' @return PSE,ME,SME using Lenth's method. Also returns a plot visualizing any factor which is significant using the obtained values of ME, and SME.
#' @importFrom stats coef model.matrix median qt
#' @importFrom ggplot2 geom_segment geom_hline coord_flip annotate labs element_blank
#' @importFrom tibble tibble
#' @importFrom unrepx PSE ME
#' @export
#' @examples m1 <- lm(ybar ~ (A+B+C+D)^2,data=adapted_epitaxial)
#' suppressWarnings(Lenth_method(m1))
#' suppressWarnings(Lenth_method(m1,alpha=0.01,method='Zahn'))
Lenth_method <- function(mod,alpha=0.05,method='Lenth',showplot=TRUE){
  if (inherits(mod, "lm")) {
    i <- pmatch("(Intercept)", names(coef(mod)))
    if (!is.na(i))
      obj <- 2 * coef(mod)[-pmatch("(Intercept)", names(coef(mod)))]
  }
  .Deprecated("pareto_plot")
  estimates <- obj
  PSE <- unrepx::PSE(estimates,method = method)
  ME <- unrepx::ME(estimates,method = method,alpha = alpha)[1]
  SME <- unrepx::ME(estimates,method = method,alpha=alpha)[2]

  results <- tibble(alpha,PSE,ME,SME)

  dat <- tibble("coeff"= factor(names(estimates),levels = rev(names(estimates))),
                "estimates" = estimates)
  dat$lower_ME = dat$estimates-ME
  dat$upper_ME = dat$estimates+ME
  dat$lower_SME = dat$estimates - SME
  dat$upper_SME = dat$estimates + SME
  if(showplot){
    lenth_plot <-  ggplot(dat, aes_string(x='coeff', y='estimates')) +
      geom_segment( aes_string(x='coeff', xend='coeff', y=0, yend='estimates'), color="grey") +
      geom_point( color="#5fad9a", size=4) +
      geom_hline(yintercept=ME, linetype='dashed', col = 'red')+
      annotate("text",x=-Inf,y=ME,hjust=-0.2,vjust=-0.5,label="ME",fontface="italic",size=2.8)+
      geom_hline(yintercept=-ME, linetype='dashed', col = 'red')+
      annotate("text",x=-Inf,y=-ME,hjust=-0.2,vjust=-0.5,label="ME",fontface="italic",size=2.8)+
      geom_hline(yintercept=SME, linetype='dotdash', col = 'blue')+
      annotate("text",x=-Inf,y=SME,hjust=-0.2,vjust=-0.5,label="SME",fontface="italic",size=2.8)+
      geom_hline(yintercept=-SME, linetype='dotdash', col = 'blue')+
      annotate("text",x=-Inf,y=-SME,hjust=-0.2,vjust=-0.5,label="SME",fontface="italic",size=2.8)+
      geom_hline(yintercept=0, linetype='dotted', col = 'black')+
      coord_flip()+
      theme_classic() %+replace%
      theme(
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
      labs(x="",y="effects")
    return(list(results = results,plot=lenth_plot))
  }
  else{
    return(list(results = results,margins_dat = dat))
  }

}
