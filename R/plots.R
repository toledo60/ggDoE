# Main Effects ------------------------------------------------------------

#' Obtain main effect plot for a factor in our design with ggplot
#'
#' @param data
#' @param factr
#' @param response
#'
#' @return data to calculate main effect, and main effect plot
#'
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




# Interaction Effects -----------------------------------------------------



#' Obtain interaction effects plot between two factors in our design with ggplot
#'
#' @param data
#' @param factr1
#' @param factr2
#' @param response
#'
#' @return data to calculate interaction effects, and interaction effects plot
#' @examples IA_effect(data = epitaxial, factr1 = A, factr2 = B, response = ybar)
#' @export
IA_effect <- function(data,factr1,factr2,response){
  var1 = dplyr::enquo(factr1)
  var2 = dplyr::enquo(factr2)
  response = enquo(response)

  dat <- data %>%
    mutate(!!quo_name(var1) := factor(!!var1), !!quo_name(var2) := factor(!!var2)) %>%
    group_by(!!var1,!!var2) %>%
    summarise(y = mean(!!response),.groups = 'drop')


  plot <- dat %>% ggplot(aes_(x=var1,y=~y,color=var2)) +
    geom_line(aes(group = {{var2}})) +
    geom_point(size=1.5)+
    theme_bw()+
    ylab(bquote("Mean of"*.(response)))+
    theme(legend.background = element_rect(fill="gray96", size=0.5, linetype="solid"))
  return(list(dat=dat,plot=plot))
}










#' Half-Normal Effects Plots
#'
#' @param obj object of class lm
#' @param alpha significance level for the Lenth method. Default is 0.05
#' @param signif_label If TRUE, only the significant factors according to the Lenth method
#' (significance level given by alpha) are labeled
#'
#' @return A dataframe with the absolute effects and half-normal qunatiles.
#' A ggplot version of halfnormal plot for factorial effects is returned
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


  plot <- ggplot(dat,aes(x= absolute_effects,
                         y = half_normal_quantiles,
                         label=effects)) +
    geom_point(color = "#1b9e77", size = 2)+
    geom_text(hjust = 0,nudge_x = 0.01*max(dat$absolute_effects),check_overlap = TRUE)+
    theme_classic()+
    labs(x="absolute effects",y="half-normal quantiles")

  return(list(dat=dat,plt=plot))
}




#' Lenth’s method: testing effect significance for experiments without variance estimates
#'
#' @param mod A linear model
#' @param alpha specify the significance level. Default is alpha=0.05
#'
#' @return PSE,ME,SME using Lenth's method. Also returns a plot visualizing any factor which is significant using the obtained
#' values of ME, and SME.
#' @export
#'
#' @examples m1 <- lm(ybar ~ (A+B+C+D)^2,data=epitaxial)
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

  dat <- tibble("coeff"= factor(names(coef(mod))[-1],levels = names(coef(mod))[-1]),
                "estimates" = coef(mod)[-1]) %>%
    mutate(lower_ME = estimates-ME,
           upper_ME = estimates+ME,
           lower_SME = estimates-SME,
           upper_SME = estimates+SME)

  lenth_plot <-  ggplot(dat, aes(x=coeff, y=estimates)) +
    geom_segment( aes(x=coeff, xend=coeff, y=0, yend=estimates), color="grey") +
    geom_point( color="#5fad9a", size=4) +
    theme_classic() +
    geom_hline(yintercept=ME, linetype='dashed', col = 'red',size=1.05)+
    annotate("text",x=-Inf,y=ME,hjust=-0.4,vjust=-0.5,label="ME",fontface="italic",size=3)+
    geom_hline(yintercept=-ME, linetype='dashed', col = 'red',size=1.05)+
    annotate("text",x=-Inf,y=-ME,hjust=-0.4,vjust=-0.5,label="ME",fontface="italic",size=3)+
    geom_hline(yintercept=SME, linetype='dotdash', col = 'blue',size=1.05)+
    annotate("text",x=-Inf,y=SME,hjust=-0.4,vjust=-0.5,label="SME",fontface="italic",size=3)+
    geom_hline(yintercept=-SME, linetype='dotdash', col = 'blue',size=1.05)+
    annotate("text",x=-Inf,y=-SME,hjust=-0.4,vjust=-0.5,label="SME",fontface="italic",size=3)+
    geom_hline(yintercept=0, linetype='dotted', col = 'black')+
    theme(
      panel.border = element_blank(),
      axis.ticks.x = element_blank()) +
    xlab("") +
    ylab("effects")
  return(list(results = results,margins_dat = dat,plot=lenth_plot))
}




