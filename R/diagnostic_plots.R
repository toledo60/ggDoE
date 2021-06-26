#' Regression Diagnostic Plots with ggplot2
#'
#' @param model Model of class "lm" or "glm"
#' @param se Display confidence interval around smooth? TRUE by default
#' @param point_size Change size of points in plots
#'
#' @return Regression Diagnostic plots
#' @importFrom broom augment
#' @importFrom ggplot2 geom_smooth stat_qq geom_abline ylim aes_string geom_text
#' @importFrom gridExtra grid.arrange
#' @importFrom stats quantile
#' @export
#'
#' @examples epitaxial_ybar <- lm(ybar ~ A+B+C+D,data=epitaxial)
#' diagnostic_plots(epitaxial_ybar)
diagnostic_plots <- function(model,se=TRUE,point_size=3.5){
  if(is.null(match("lm",c("glm","lm")))){
    stop("model should be of class lm or glm")
  }else{

    df = broom::augment(model)
    p = length(coef(model))
    n = nrow(df)

    res =  df$.resid
    df$sqrt_abs_stdres = sqrt(abs(df$.std.resid))
    df$leverage = ifelse(df$.hat  > 2 * p / n,rownames(df),"")
    df$outlier = ifelse(abs(df$.std.resid) > 3,rownames(df),"")



    # Residuals vs fitted -----------------------------------------------------

    limit = max(abs(res))
    margin_factor = 5
    margin = round(limit / margin_factor)

    res_fitted_base <- ggplot(data = df, aes_string(y = '.resid', x = '.fitted')) +
      geom_point(size=point_size,shape=1) +
      geom_smooth(fill="#d9d9d9",se=se,color = "indianred3",size=1.1)+
      labs(y = "Residuals", x = "Fitted Values",title = "Residual vs. Fitted Value") +
      ylim(-(limit + margin), limit + margin) +
      theme_bw()
    if(sum(abs(df$.std.resid) > 3)==0){
      res_fitted <- res_fitted_base
    }
    else{

      res_fitted <- res_fitted_base +
        geom_point(size=point_size,shape=1,
                   color= ifelse(abs(df$.std.resid) > 3,"indianred3","black")) +
        geom_text(aes_string(label='outlier'),vjust = 0,
                  nudge_y = 0.5,color='indianred3')
    }


    # QQ-plot -----------------------------------------------------------------

    slope = (quantile(res, .75) - quantile(res, .25)) / (qnorm(.75) - qnorm(.25))
    intercept = quantile(res,.25) - slope*qnorm(.25)
    qq_line = data.frame(intercept = intercept, slope = slope)

    qq_plot <- ggplot(data = model) +
      stat_qq(aes_string(sample = 'res'), size=point_size,shape=1) +
      labs(x = "Theoretical Quantile", y = "Standardized Residual",
           title = "Normal-QQ Plot") +
      geom_abline(data = qq_line ,aes(intercept = intercept ,slope = slope),
                  color = "indianred3", size = 1.1)+
      theme_bw()



    # Scale-Location ----------------------------------------------------------

    stdres_fitted <- ggplot(data = df, aes_string(y = 'sqrt_abs_stdres', x = '.fitted')) +
      geom_point(size = point_size,shape=1) +
      geom_smooth(method = 'loess',se=se, size = 1.1, color = "indianred3",fill="#d9d9d9") +
      labs(y=expression(sqrt("|Standardized Residuals|")), x = "Fitted Values",
           title = "Scale-Location Plot")+
      theme_bw()


    # Residual vs Leverage ----------------------------------------------------


    stdres_leverage_base <- ggplot(data = df, aes_string(x = '.hat', y = '.std.resid')) +
      geom_point(size = point_size,shape=1) +
      geom_smooth(method = 'loess',se=se, color = "indianred3" ,fill="#d9d9d9",size = 1.1) +
      labs(y = "Standardized Residuals", x = "Leverage",
           title = 'Residual vs. Leverage')+
      theme_bw()
    if(sum(df$.hat  > 2 * p / n) == 0){
      stdres_leverage <- stdres_leverage_base
    }else{


      stdres_leverage <- stdres_leverage_base+
        geom_point(size=point_size,shape=1,
                   color= ifelse(df$.hat  > 2 * p / n,"indianred3","black"))+
        geom_text(aes_string(label='leverage'),
                  vjust = 0, nudge_y = 0.1,color='indianred3')
    }

    return(suppressMessages(gridExtra::grid.arrange(res_fitted,
                                                    qq_plot,stdres_fitted,
                                                    stdres_leverage,ncol=2)))
  }
}
