#' Half-Normal Effects Plots
#'
#' @param obj object of class lm
#' @param alpha significance level for the Lenth method. Default is 0.05
#' @param signif_label If TRUE, only the significant factors according to the Lenth method
#' (significance level given by alpha) are labeled
#'
#' @return A dataframe with the absolute effects and half-normal qunatiles.
#' A ggplot version of halfnormal plot for factorial effects is returned
#' @importFrom ggplot2 ggplot aes geom_point geom_text theme_classic labs
#' @importFrom stats qnorm
#' @importFrom utils tail
#' @export
#'
#' @examples m1 <- lm(lns2 ~ (A+B+C+D)^4,data=epitaxial)
#' halfnormal(m1)
#' halfnormal(m1,alpha=0.1)
#' halfnormal(m1,alpha=0.2,signif_label=TRUE)
halfnormal <- function(obj,alpha=0.05,signif_label=FALSE){
  if (inherits(obj, "lm")) {
    i <- pmatch("(Intercept)", names(coef(obj)))
    if (!is.na(i))
      effects <- coef(obj)[-pmatch("(Intercept)", names(coef(obj)))]
    obj <- 2 * effects
  }
  b <- obj
  m <- length(b)
  d <- m/3
  s0 <- 1.5 * median(abs(b))
  cj <- as.numeric(b[abs(b) < 2.5 * s0])
  PSE <- 1.5 * median(abs(cj))
  ME <- qt(1 - alpha/2, d) * PSE

  effs <- sort(abs(obj))
  names <- names(effs)
  r <- c(1:m)
  zscore<-c(rep(0,m))
  for (i in 1:m) {
    zscore[i]<-qnorm( ( ( r[i]-.5)/m+1)/2 )
    if(signif_label){
      logc<-(abs(effs[i])<= ME)
      if (logc) {names[i]<-" "}}
  }

  dat <- tibble("effects"=names,
                "absolute_effects"=effs,
                "half_normal_quantiles"=zscore
  )


  plot <- ggplot(dat,aes_string(x= 'absolute_effects',
                         y = 'half_normal_quantiles',
                         label='effects')) +
    geom_point(color = "#1b9e77", size = 2)+
    geom_text(hjust = 0,nudge_x = 0.01*max(dat$absolute_effects),check_overlap = TRUE)+
    theme_classic()+
    labs(x="absolute effects",y="half-normal quantiles")

  return(list(dat=dat,plot=plot))
}
