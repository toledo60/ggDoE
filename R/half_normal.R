#' Half-Normal Effects Plots
#'
#' @param obj object of class lm
#' @param method Character value. Method to calculate PSE. Default is Lenth. Options include: Zahn, WZahn, Lenth, RMS, Dong, JuanPena, Daniel
#' @param alpha specify the significance level. Default is alpha=0.05
#' @param label_active If TRUE, active effects are labeled if the effects cross the computed margin of error (ME). See method argument for more details
#' @param ref_line Dafault is TRUE, if FALSE the abline with slope (1/PSE) is not displayed. Reference line should follow along most points that are not considered outliers.
#' @param showME Default is FALSE, if TRUE the cutoffs for margin of errors (ME) and simultaneous margin of error (SME) are shown
#' @param showplot Default is TRUE, if FALSE plot will not be shown and a tibble is returned used to create the plot along with the calculated PSE,ME,SME
#' @return A tibble with the absolute effects and half-normal quantiles. A ggplot2 version of halfnormal plot for factorial effects is returned
#' @importFrom ggplot2 ggplot aes geom_point geom_text theme_bw labs geom_vline annotate geom_abline element_blank
#' @importFrom stats qnorm coef
#' @importFrom utils tail
#' @importFrom unrepx PSE ME
#' @importFrom tibble tibble
#' @export
#'
#' @examples m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
#' half_normal(m1)
#' half_normal(m1,alpha=0.1,label_active=TRUE,showME=TRUE)
#' half_normal(m1,method='Zahn',alpha=0.1,ref_line=TRUE,label_active=TRUE,showME=TRUE)
half_normal <- function(obj,method='Lenth',
                       alpha=0.05,
                       label_active=FALSE,
                       ref_line = FALSE,
                       showME = FALSE,
                       showplot=TRUE){
  if (inherits(obj, "lm")) {
    i <- pmatch("(Intercept)", names(coef(obj)))
    if (!is.na(i))
      effects <- coef(obj)[-pmatch("(Intercept)", names(coef(obj)))]
    obj <- 2 * effects
  }
  estimates <- obj
  PSE <- unrepx::PSE(estimates,method = method)
  ME <- unrepx::ME(estimates,method = method,alpha = alpha)[1]
  SME <- unrepx::ME(estimates,method = method,alpha=alpha)[2]

  effs <- sort(abs(estimates))
  names <- names(effs)
  m <- length(estimates)
  r <- c(1:m)
  zscore<-c(rep(0,m))

  for (i in 1:m) {
    zscore[i]<-qnorm( ( ( r[i]-.5)/m+1)/2 )
    if(label_active){
      logc<-(abs(effs[i])<= ME)
      if (logc) {names[i]<-NA}}
  }

  dat <- tibble::tibble("effects"=names,
                        "absolute_effects"=effs,
                        "half_normal_quantiles"=zscore
  )

  if(showplot){
    base_plot <- ggplot(dat,aes_string(x= 'absolute_effects',
                                       y = 'half_normal_quantiles',
                                       label='effects')) +
      geom_point(color = "#1b9e77", size = 2.5)+
      geom_text(hjust = 0,nudge_x = 0.01*max(dat$absolute_effects),
                check_overlap = TRUE,na.rm = TRUE)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      labs(x="absolute effects",y="half-normal quantiles")

    if(ref_line & showME){
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
    else if (!ref_line & !showME){
      plt <- base_plot
    }
    else if(!ref_line & showME){
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
    else if(ref_line & !showME){
      plt <- base_plot + geom_abline(intercept=0,slope = 1/PSE,linetype=2)
    }
    return(plt)
  }
  else{
    return(dat)
  }
}
