#' Lenth’s method: testing effect significance for experiments without variance estimates
#'
#' @param mod A linear model
#' @param alpha specify the significance level. Default is alpha=0.05
#'
#' @return PSE,ME,SME using Lenth's method. Also returns a plot visualizing any factor which is significant using the obtained
#' values of ME, and SME.
#' @importFrom stats coef model.matrix median qt
#' @importFrom ggplot2 geom_segment geom_hline coord_flip annotate xlab ylab element_blank
#' @importFrom tibble tibble
#' @export
#' @examples m1 <- lm(ybar ~ (A+B+C+D)^2,data=adapted_epitaxial)
#' Lenth_method(m1)
#' Lenth_method(m1,alpha=0.01)
Lenth_method <- function(mod,alpha=0.05){
  if (inherits(mod, "lm")) {
    i <- pmatch("(Intercept)", names(coef(mod)))
    if (!is.na(i))
      obj <- 2 * coef(mod)[-pmatch("(Intercept)", names(coef(mod)))]
  }
  b <- obj
  m <- length(b)
  d <- m/3
  s0 <- 1.5 * median(abs(b))
  cj <- as.numeric(b[abs(b) < 2.5 * s0])
  PSE <- 1.5 * median(abs(cj))

  ME <- qt(1 - alpha/2, d) * PSE
  gamma <- (1 + (1 - alpha)^(1/m))/2
  SME <- qt(gamma, d) * PSE

  results <- tibble(alpha,PSE,ME,SME)

  dat <- tibble("coeff"= factor(names(coef(mod))[-1],levels = rev(names(coef(mod))[-1])),
                "estimates" = 2*coef(mod)[-1])
  dat$lower_ME = dat$estimates-ME
  dat$upper_ME = dat$estimates+ME
  dat$lower_SME = dat$estimates - SME
  dat$upper_SME = dat$estimates + SME

  lenth_plot <-  ggplot(dat, aes_string(x='coeff', y='estimates')) +
    geom_segment( aes_string(x='coeff', xend='coeff', y=0, yend='estimates'), color="grey") +
    geom_point( color="#5fad9a", size=4) +
    geom_hline(yintercept=ME, linetype='dashed', col = 'red',size=1.04)+
    annotate("text",x=-Inf,y=ME,hjust=-0.2,vjust=-0.5,label="ME",fontface="italic",size=2.8)+
    geom_hline(yintercept=-ME, linetype='dashed', col = 'red',size=1.04)+
    annotate("text",x=-Inf,y=-ME,hjust=-0.2,vjust=-0.5,label="ME",fontface="italic",size=2.8)+
    geom_hline(yintercept=SME, linetype='dotdash', col = 'blue',size=1.04)+
    annotate("text",x=-Inf,y=SME,hjust=-0.2,vjust=-0.5,label="SME",fontface="italic",size=2.8)+
    geom_hline(yintercept=-SME, linetype='dotdash', col = 'blue',size=1.04)+
    annotate("text",x=-Inf,y=-SME,hjust=-0.2,vjust=-0.5,label="SME",fontface="italic",size=2.8)+
    geom_hline(yintercept=0, linetype='dotted', col = 'black')+
    coord_flip()+
    theme_classic() %+replace%
    theme(
      panel.border = element_blank(),
      axis.ticks.x = element_blank()) +
    xlab("") +
    ylab("effects")
  return(list(results = results,margins_dat = dat,plot=lenth_plot))
}
