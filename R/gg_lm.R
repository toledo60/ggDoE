#' Regression Diagnostic Plots with ggplot2
#'
#' @param model Model of class "lm" or "glm"
#' @param which_plots Choose which diagnostic plots to choose from. \cr Options are 1 = 'residual vs fitted', 2 = 'Normal-QQ',
#' 3 = 'Scale-location', 4 = 'Residual vs Leverage', 5 = "Cook's Distance". 6 = "Collinearity". Default is 1:4
#' @param cooksD_type An integer between 1 and 4 indicating the threshold to be computed for Cook's Distance plot. Default is 1. See details for threshold computation
#' @param standard_errors Display confidence interval around geom_smooth, FALSE by default
#' @param point_size Change size of points in plots
#' @param theme_color Change color of the geom_smooth line and text labels for the respective diagnostic plot
#' @param n_columns number of columns for grid layout. Default is 2
#' @return Regression diagnostic plots
#' @importFrom ggplot2 geom_smooth stat_qq geom_abline ylim aes sym geom_linerange geom_hline
#' @importFrom stats quantile lm.influence cooks.distance rstandard as.formula model.matrix
#' @export
#'
#' @details
#'
#' \strong{Plot 5:} "Cook's Distance": A data point having a large Cook's distance indicates that the data point
#' strongly influences the fitted values of the model. The default threshold used for detecting or classifying observations as outers is \eqn{4/n} (i.e cooksD_type=1)
#' where \eqn{n} is the number of observations. The thresholds computed are as follows: \cr
#' \itemize{
#' \item{cooksD_type = 1: }{4/n}
#' \item{cooksD_type = 2: }{4/(n-p-1)}
#' \item{cooksD_type = 3: }{1/(n-p-1)}
#' \item{cooksD_type = 4: }{3* mean(cook's distance values)}
#' }
#' where \eqn{n} is the number of observations and \eqn{p} is the number of predictors. \cr
#'
#' \strong{Plot 6:} "Collinearity": Conisders the variance inflation factor (VIF) for multicollinearity: \cr
#' Tolerance = \eqn{1 - R_j^2}, VIF = (1/Tolerance)
#' where \eqn{R_j^2} is the coefficient of determination of a regression of predictor \eqn{j} on all the other predictors.
#' A general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 indicates a multicollinearity problem \cr \cr
#'
#' @references
#' Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. New York: John Wiley & Sons.\cr
#'
#' Sheather, S. (2009). A modern approach to regression with R. Springer Science & Business Media.
#'
#' @examples
#' model <- lm(mpg ~ wt + am + gear, data = mtcars)
#' gg_lm(model)
gg_lm <- function(model,which_plots = 1:4,
                  cooksD_type = 1,
                  standard_errors=FALSE,
                  point_size=1.5,
                  theme_color = "#21908CFF",
                  n_columns=2){
  if(!insight::is_regression_model(model)){
    stop("model should be a regression model of class 'lm'")
  }else{

    insight::check_if_installed(c('patchwork','ggrepel'))
    df <- model$model
    df$.std.resid <- rstandard(model)

    if(sum(is.na(df$.std.resid)) > 0){
      stop("Insufficient degrees of freedom, check your model. Can't obtain diagnostic plots")
    }

    df$.fitted <- model$fitted.values
    df$.resid <- model$residuals
    df$.hat <- lm.influence(model)$hat
    df$.sigma <- lm.influence(model)$sigma
    df$.cooksd <- cooks.distance(model)

    p <- length(coef(model))
    n <- nrow(df)
    row_names_df <- rownames(df)

    df$sqrt_abs_stdres <- sqrt(abs(df$.std.resid))
    df$leverage <- ifelse(df$.hat  > 2 * p / n,row_names_df,NA)
    df$outlier <- ifelse(abs(df$.std.resid) > 3,row_names_df,NA)

    plot_list <- vector(mode='list',length = 6)

    # Residuals vs fitted -----------------------------------------------------

    limit <- max(abs(df$.resid))
    margin_factor <- 5
    margin <- round(limit / margin_factor)

    res_fitted_base <- ggplot(data = df,
                              aes(y = !!sym('.resid'), x = !!sym('.fitted'))) +
      geom_point(size=point_size,shape=1) +
      geom_smooth(fill="#d9d9d9",se=standard_errors,
                  color = theme_color,linewidth=1.1,
                  method = 'loess',formula = 'y ~ x')+
      geom_hline(yintercept = 0,linetype='dashed')+
      labs(y = "Residuals", x = "Fitted Values",
           title = "Residual vs. Fitted Value") +
      ylim(-(limit + margin), limit + margin) +
      theme_bw_nogrid()

    if(sum(abs(df$.std.resid) > 3)==0){
      res_fitted <- res_fitted_base
    }
    else{

      res_fitted <- res_fitted_base +
        geom_point(size=point_size,shape=1,
                   color= ifelse(abs(df$.std.resid) > 3,
                                 theme_color,"black")) +
        ggrepel::geom_label_repel(data = df,aes(label=!!sym('outlier')),
                                  na.rm = TRUE,
                                  max.overlaps = 20,
                                  color="#21908CFF")

    }

    # QQ-plot -----------------------------------------------------------------

    slope <- (quantile(df$.resid, .75) - quantile(df$.resid, .25)) / (qnorm(.75) - qnorm(.25))
    intercept <- quantile(df$.resid,.25) - slope*qnorm(.25)
    qq_line <- data.frame(intercept = intercept, slope = slope)

    qq_plot <- ggplot(data = model) +
      stat_qq(aes(sample = df$.resid), size=point_size,shape=1) +
      labs(x = "Theoretical Quantile", y = "Standardized Residual",
           title = "Normal-QQ Plot") +
      geom_abline(data = qq_line ,aes(intercept = intercept ,slope = slope),
                  color = theme_color, linewidth = 1.1)+
      theme_bw_nogrid()


    # Scale-Location ----------------------------------------------------------

    stdres_fitted <- ggplot(data = df, aes(y = !!sym('sqrt_abs_stdres'),
                                           x = !!sym('.fitted'))) +
      geom_point(size = point_size,shape=1) +
      geom_smooth(method = 'loess',se=standard_errors,
                  linewidth = 1.1, color = theme_color,
                  fill="#d9d9d9",formula = 'y ~ x') +
      labs(y=expression(sqrt("|Standardized Residuals|")),
           x = "Fitted Values",
           title = "Scale-Location Plot")+
      theme_bw_nogrid()

    # Cooks Distance ----------------------------------------------------------

    h <- switch(cooksD_type,
                '1' = 4/n,
                '2' = 4 / (n - p - 1),
                '3' = 1 / (n - p - 1),
                '4' = 3 * mean(df$.cooksd))

    df$cooksD <- ifelse(df$.cooksd > h,row_names_df,NA)

    limit <- max(df$.cooksd, na.rm = T)
    margin <- round(limit / margin_factor)
    max_cook <- limit + margin

    .cooksd <- NULL

    cooksD_plot <- ggplot(data = df, aes(x=1:n,.cooksd, ymin = 0,
                                         ymax = df$.cooksd)) +
      geom_point(size=point_size,shape=1) +
      geom_hline(yintercept = h,
                 color= theme_color,linetype=2)+
      geom_linerange(color='#bfbfbf') +
      ggrepel::geom_label_repel(data = df,aes(label=!!sym('cooksD')),
                                na.rm = TRUE,
                                max.overlaps = 20,
                                color=theme_color)+
      labs(x= 'Observarion',y="Cook's Distance",
           title="Cook's Distance Plot")+
      theme_bw_nogrid()


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
        geom_point(shape = 1, size = point_size) +
        geom_linerange(color='#bfbfbf')+
        geom_hline(yintercept = 5,
                   color= theme_color,linetype=2)+
        geom_hline(yintercept = 10,
                   color= theme_color,linetype=2)+
        ggrepel::geom_label_repel(data = results,
                                  aes(label=!!sym('high_VIF')),
                                  na.rm = TRUE,
                                  max.overlaps = 20,
                                  color=theme_color)+
        labs(x = "", y = "Variance Inflation Factor (VIF)",
             title = "Collinearity")+
        theme_bw_nogrid()

      return(plt)
    }

    # Residual vs Leverage ----------------------------------------------------

    if( length(unique(round(df$.hat,4)))!=1 ){

      stdres_leverage_base <- ggplot(data = df, aes(x = !!sym('.hat'),
                                                    y = !!sym('.std.resid'))) +
        geom_point(size = point_size,shape=1) +
        geom_smooth(method = 'loess',se=standard_errors,
                    color = theme_color ,fill="#d9d9d9",
                    linewidth = 1.1,formula = 'y ~ x') +
        labs(y = "Standardized Residuals", x = "Leverage",
             title = 'Residual vs. Leverage')+
        theme_bw_nogrid()

      if(sum(df$.hat  > 2 * p / n) == 0){
        stdres_leverage <- stdres_leverage_base
      }else{

        stdres_leverage <- stdres_leverage_base+
          geom_point(size=point_size,shape=1,
                     color= ifelse(df$.hat  > 2 * p / n,theme_color,"black"))+
          ggrepel::geom_label_repel(data = df,aes(label= !!sym('leverage')),
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


      return(patchwork::wrap_plots(plot_list[which_plots],
                                   ncol = n_columns))

    }
    else{

      plot_list[[1]] <- res_fitted + labs(title = "Residual vs.\nFitted Value")
      plot_list[[2]] <- qq_plot
      plot_list[[3]] <- stdres_fitted
      plot_list[[4]] <- cooksD_plot
      plot_list[[5]] <- vif_plot(model,point_size=point_size,
                                 theme_color = theme_color)

      return(patchwork::wrap_plots(plot_list[which_plots],
                                   ncol = n_columns))
    }
  }
}
