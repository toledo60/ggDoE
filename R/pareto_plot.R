#' Pareto Plot of Effects
#'
#' @param obj object of class lm
#' @param alpha significance level to compute margin of errors based on Lenth method. Default is alpha=0.05
#' @param showplot Default is TRUE, if false plot will not be shown and a tibble is returned with data used to create the pareto plot
#' @param showME Default is TRUE, if false the cutoffs for margin of errors (ME) and simultaneous margin of error (SME) are not shown
#' @param method Character value. Method to calculate PSE. Default is Lenth. Options include: Zahn, WZahn, Lenth, RMS, Dong, JuanPena, Daniel
#' @return A bar plot with ordered effects, margin of error (ME) and simultaneous margin of error (SME) cutoffs.
#' @importFrom stats reorder coef
#' @importFrom ggplot2 geom_hline geom_bar annotate labs coord_flip theme_classic aes_string
#' @export
#'
#' @examples
#' m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
#' pareto_plot(m1)
#' pareto_plot(m1,method='Zahn',alpha=0.1)
#' pareto_plot(m1,showME=FALSE)
pareto_plot <- function(obj,alpha=0.05,method='Lenth',showME=TRUE,showplot=TRUE){
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

  dat <- tibble::tibble("effect_names"=factor(names(estimates)),
                "effects"=estimates,
                'abs_effects' = abs(estimates),
                "cols" = ifelse(effects >0,'#d9a698','#9ecede'))

  sorted_dat <- dat[order(dat$abs_effects),]

  if(showplot){
    base_plot <- ggplot(sorted_dat,
                        aes_string(x= paste0("reorder(",'effect_names',", abs_effects)"),
                                  y = 'abs_effects',
                                  fill= 'cols')) +
      geom_bar(stat = "identity")+
      theme_classic()+
      coord_flip()+
      theme(legend.position = "none")
    if(showME){
      plot <- base_plot +
        geom_hline(yintercept = ME,linetype=2)+
        annotate("text",x=-Inf,y=ME,hjust=-0.2,vjust=-0.5,
                 label="ME",fontface="italic",size=2.8)+
        geom_hline(yintercept = SME,linetype=2)+
        annotate("text",x=-Inf,y=SME,hjust=-0.2,vjust=-0.5,
                 label="SME",fontface="italic",size=2.8)+
        labs(x="",y='absolute effects',caption = paste0(method," ME=",round(ME,2),",",
                                                        " SME=",round(SME,2)))
    }
    else{
      plot <- base_plot + labs(x="",y='absolute effects')
    }

    return(plot)
  }else{
    return(list(errors = tibble::tibble(alpha,PSE,ME,SME), dat = sorted_dat[,c(1,2,3)]))
  }
}
