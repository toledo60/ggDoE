#' Regression Diagnostic Plots with ggplot2
#'
#' @param model Model of class "lm" or "glm"
#' @param standard_errors Display confidence interval around geom_smooth, FALSE by default
#' @param point_size Change size of points in plots
#' @param theme_color Change color of the geom_smooth line and text labels for the respective diagnostic plot
#' @param which_plots Choose which diagnostic plots to choose from. Options are 1 = 'residual vs fitted', 2 = 'Normal-QQ',
#' 3 = 'Scale-location', 4 = 'Residual vs Leverage', 5 = "Cook's Distance". 6 = "Collinearity". Default is 1:4
#' @param ncols number of columns for grid layout. Default is 2
#' @return Regression diagnostic plots
#' @importFrom ggplot2 geom_smooth stat_qq geom_abline ylim aes_string theme_bw geom_linerange element_blank geom_hline
#' @importFrom stats quantile lm.influence cooks.distance rstandard as.formula model.matrix
#' @export
#'
#' @examples
#' model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)
#' diagnostic_plots(model,which_plots=1:6)
diagnostic_plots <- function(model,standard_errors=FALSE,
                             point_size=1.5,
                             theme_color = "#21908CFF",
                             which_plots = 1:4,
                             ncols=2){
  if (!inherits(model, "lm")) {
    stop("model should be of class lm or glm")
  }else{

    df <- model$model
    df$.fitted <- model$fitted.values
    df$.resid <- model$residuals
    df$.hat <- lm.influence(model)$hat
    df$.sigma <- lm.influence(model)$sigma
    df$.cooksd <- cooks.distance(model)
    df$.std.resid <- rstandard(model)

    if(sum(is.na(df$.std.resid)) > 0){
      stop("Insufficient degrees of freedom, check your model. Can't obtain diagnostic plots")
    }

    p <- length(coef(model))
    n <- nrow(df)

    res <-  df$.resid
    df$sqrt_abs_stdres <- sqrt(abs(df$.std.resid))
    df$leverage <- ifelse(df$.hat  > 2 * p / n,rownames(df),NA)
    df$outlier <- ifelse(abs(df$.std.resid) > 3,rownames(df),NA)

    plot_list <- list()

    # Residuals vs fitted -----------------------------------------------------

    limit <- max(abs(res))
    margin_factor <- 5
    margin <- round(limit / margin_factor)

    res_fitted_base <- ggplot(data = df, aes_string(y = '.resid', x = '.fitted')) +
      geom_point(size=point_size,shape=1) +
      geom_smooth(fill="#d9d9d9",se=standard_errors,color = theme_color,size=1.1)+
      labs(y = "Residuals", x = "Fitted Values",title = "Residual vs. Fitted Value") +
      ylim(-(limit + margin), limit + margin) +
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(sum(abs(df$.std.resid) > 3)==0){
      res_fitted <- res_fitted_base
    }
    else{

      res_fitted <- res_fitted_base +
        geom_point(size=point_size,shape=1,
                   color= ifelse(abs(df$.std.resid) > 3,theme_color,"black")) +
        ggrepel::geom_label_repel(data = df,aes_string(label='outlier'),na.rm = TRUE,
                                  max.overlaps = 20,
                                  color="#21908CFF")

    }


    # QQ-plot -----------------------------------------------------------------

    slope <- (quantile(res, .75) - quantile(res, .25)) / (qnorm(.75) - qnorm(.25))
    intercept <- quantile(res,.25) - slope*qnorm(.25)
    qq_line <- data.frame(intercept = intercept, slope = slope)

    qq_plot <- ggplot(data = model) +
      stat_qq(aes_string(sample = 'res'), size=point_size,shape=1) +
      labs(x = "Theoretical Quantile", y = "Standardized Residual",
           title = "Normal-QQ Plot") +
      geom_abline(data = qq_line ,aes(intercept = intercept ,slope = slope),
                  color = theme_color, size = 1.1)+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())



    # Scale-Location ----------------------------------------------------------

    stdres_fitted <- ggplot(data = df, aes_string(y = 'sqrt_abs_stdres', x = '.fitted')) +
      geom_point(size = point_size,shape=1) +
      geom_smooth(method = 'loess',se=standard_errors,
                  size = 1.1, color = theme_color,fill="#d9d9d9") +
      labs(y=expression(sqrt("|Standardized Residuals|")), x = "Fitted Values",
           title = "Scale-Location Plot")+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    # Cooks Distance ----------------------------------------------------------

    cooksd <- cooks.distance(model)
    h <- 4/n
    df$cooksD <- ifelse(cooksd > h,rownames(df),
                        NA)

    limit <- max(cooksd, na.rm = T)
    margin_factor <- 5
    margin <- round(limit / margin_factor)
    max_cook <- limit + margin

    .cooksd <- NULL

    cooksD_plot <- ggplot(data = df, aes(x=1:n,.cooksd, ymin = 0,
                                         ymax = cooksd)) +
      geom_point(size=point_size,shape=1) +
      geom_hline(yintercept = h,
                 color= theme_color,linetype=2)+
      geom_linerange(color='#bfbfbf') +
      ggrepel::geom_label_repel(data = df,aes_string(label='cooksD'),na.rm = TRUE,
                                max.overlaps = 20,
                                color=theme_color)+
      labs(x= 'Observarion',y="Cook's Distance",
           title="Cook's Distance Plot")+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    # Variance Inflation Factor -----------------------------------------------

    vif_plot <- function(model,point_size=point_size,
                         theme_color = theme_color) {
      m   <- as.data.frame(model.matrix(model))[, -1]
      vars <- names(m)
      p   <- length(model$coefficients) - 1
      tolerane <- c()

      regress_i <- function(vars, data, i) {

        fm <- as.formula(paste0("`", vars[i], "` ", "~ ."))
        R2 <- summary(lm(fm, data = data))$r.squared

        return(1 - R2)
      }

      for (i in seq_len(p)) {
        tolerane[i] <- regress_i(vars, m, i)
      }

      vifs <- 1 / tolerane

      results <- data.frame(Variables = vars,
                            VIF = vifs)

      results$high_VIF <- ifelse(results$VIF  >= 5,
                                 results$Variables,NA)

      VIF <- NULL
      Variables <- NULL

      plt <- ggplot(results)+
        aes(x = Variables, y = VIF,
            ymin=0,ymax=VIF) +
        geom_point(shape = 1, size = point_size,
                   colour = theme_color) +
        geom_linerange(color='#bfbfbf')+
        geom_hline(yintercept = 5,
                   color= theme_color,linetype=2)+
        geom_hline(yintercept = 10,
                   color= theme_color,linetype=2)+
        ggrepel::geom_label_repel(data = results,aes_string(label='high_VIF'),
                                  na.rm = TRUE,
                                  max.overlaps = 20,
                                  color=theme_color)+
        labs(x = "", y = "Variance Inflation Factor (VIF)",
             title = "Collinearity")+
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      return(plt)
    }


    # Residual vs Leverage ----------------------------------------------------

    if( length(unique(round(df$.hat,4)))!=1 ){

      stdres_leverage_base <- ggplot(data = df, aes_string(x = '.hat',
                                                           y = '.std.resid')) +
        geom_point(size = point_size,shape=1) +
        geom_smooth(method = 'loess',se=standard_errors,
                    color = theme_color ,fill="#d9d9d9",
                    size = 1.1) +
        labs(y = "Standardized Residuals", x = "Leverage",
             title = 'Residual vs. Leverage')+
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      if(sum(df$.hat  > 2 * p / n) == 0){
        stdres_leverage <- stdres_leverage_base
      }else{

        stdres_leverage <- stdres_leverage_base+
          geom_point(size=point_size,shape=1,
                     color= ifelse(df$.hat  > 2 * p / n,theme_color,"black"))+
          ggrepel::geom_label_repel(data = df,aes_string(label='leverage'),
                                    na.rm = TRUE,
                                    max.iter = 20,
                                    color=theme_color)
      }

      plot_list[[1]] <- res_fitted
      plot_list[[2]] <- qq_plot
      plot_list[[3]] <- stdres_fitted
      plot_list[[4]] <- stdres_leverage
      plot_list[[5]] <- cooksD_plot
      plot_list[[6]] <- vif_plot(model,point_size=point_size,
                                 theme_color = theme_color)



      return(suppressMessages(gridExtra::grid.arrange(grobs=plot_list[which_plots],
                                                      ncol=ncols)))

    }
    else{

      plot_list[[1]] <- res_fitted + labs(title = "Residual vs.\nFitted Value")
      plot_list[[2]] <- qq_plot
      plot_list[[3]] <- stdres_fitted
      plot_list[[4]] <- cooksD_plot
      plot_list[[5]] <- vif_plot(model,point_size=point_size,
                                 theme_color = theme_color)


      return(suppressMessages(gridExtra::grid.arrange(grobs=plot_list[which_plots],
                                                      ncol=ncols)))
    }
  }
}
