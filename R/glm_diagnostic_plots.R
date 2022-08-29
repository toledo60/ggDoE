#' GLM Diagnostic Plots with ggplot2
#'
#' @param model Model of class "glm"
#' @param discrete_edm Logical value to exclusively specify if a discrete EDM is chosen to build model. Quantile residuals are used instead of Deviance/Pearson residuals for the plots for Discrete EDMs
#' @param which_plots Choose which diagnostic plots to choose from. \cr Options are 1 = "Residuals vs Fitted"; 2 = "Working Responses vs Linear Predictors"; 3 = "Normal Q-Q"; 4 = "Outlier Detection"; 5 = "Half norm plot using leverages"; 6 = "Half norm plot using Cook's Distance"; 7 = "Cook's Distance"; 8 = "DFFITS"; 9 = "VIF"
#' @param n_columns number of columns for grid layout. Default is 2
#' @param standard_errors Display confidence interval around geom_smooth, FALSE by default
#' @param theme_color Change color of the geom_smooth line and text labels for the respective diagnostic plot
#' @param point_size Change size of points in plots
#'
#' @return GLM diagnostic plots
#' @importFrom ggplot2 geom_smooth stat_qq geom_abline ylim aes_string theme_bw geom_linerange element_blank geom_hline
#' @importFrom stats quantile lm.influence cooks.distance rstandard as.formula model.matrix
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @details
#'
#' \bold{Check the assumptions for the systematic component of the GLM}:
#'
#' \emph{Plot 1}: If any trends appear, then the systematic component can be improved. This usually involves doing 1 or a combination of the following: (1) changing the link fucntion, (2) adding new predictor variables, and/or (3) transforming the current predictor variables in the model.
#'
#' \emph{Plot 2}: If plot is not roughly linear, then another link function might be more appropriate.
#'
#' \bold{Check the assumptions for the random component - EDM distribution - of the GLM}:
#'
#' \emph{Plot 3}: If most points are NOT close to the line, then the EDM distribution may not be appropriate. The quantile residuals are used since they have an exact normal distribution if the appropriate EDM has been chosen.
#'
#' \bold{Check for outliers and influential points}:
#'
#' \emph{Plot 4}: If |residual| > 3, then the observation is flagged as an outlier.
#'
#' \emph{Plot 5 & 6}: Observations that deviate too much from red line are potential outliers.
#'
#' \emph{Plot 7}: Check for outliers with Cook's Distance. A data point having a large Cook's distance indicates that the data point strongly influences the fitted values of the model. If Cook's Distance > \eqn{4 / observations}, then the observation is flagged as influential.
#'
#' \emph{Plot 8}: Check for outliers with DFFITS, which considers how much the fitted value of observation i changes between the model fitted with all the data and the model fitted with obervation i omitted. If |DFFITS| > \eqn{2 * sqrt((parameters + 1) / (observations - parameters - 1))}, then the observation is flagged as influential.
#'
#' \bold{Check for collinearity}:
#'
#' \emph{Plot 9}: Check for collinearity with the variance inflation factor (VIF). Tolerance = \eqn{1 - R_j^2}, VIF = \eqn{1 / Tolerance}, where \eqn{R_j^2} is the coefficient of determination of a regression of predictor j on all the other predictors. A general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 indicates a multicollinearity problem.
#'
#' @references
#'
#' Weisberg, S.: Applied Linear Regression. John Wiley and Sons, New York (1985)
#'
#' Pierce, D.A., Shafer, D.W.: Residuals in generalized linear models. Journal of the American Statistical Association 81, 977–986 (1986)
#'
#' Williams, D.A.: Generalized linear models diagnostics using the deviance and single-case deletions. Applied Statistics 36(2), 181–191 (1987)
#'
#' McCullagh, P., Nelder, J.A.: Generalized Linear Models, second edn. Chapman and Hall, London (1989)
#'
#' Dunn, P.K., Smyth, G.K.: Randomized quantile residuals. Journal of Computational and Graphical Statistics 5(3), 236–244 (1996)
#'
#' Dunn, P.K., Smyth, G.K. Generalized Linear Models with Examples in R. Springer Texts in Statistics, 297-327 (2018)
#'
#' @author
#'
#' Mo Amiri \email{moamiristat@@protonmail.com}
#'
#' @examples
#' model <- glm(Volume ~ Girth + Height, family = Gamma(link = "log"), data = trees)
#'
#' # Default plots returned
#' glm_diagnostic_plots(model, discrete_edm = FALSE, which_plots = 1:4)
#'
#' # Half norm plots
#' glm_diagnostic_plots(model, discrete_edm = FALSE, which_plots = 5:6)
#'
#' # Cook's Distance & DFFITS
#' glm_diagnostic_plots(model, discrete_edm = FALSE, which_plots = 7:8)
#'
#' # Collinearity
#' glm_diagnostic_plots(model, discrete_edm = FALSE, which_plots = 9, n_columns = 1)
glm_diagnostic_plots <- function(model,
                                 discrete_edm,
                                 which_plots = 1:4,
                                 n_columns = 2,
                                 standard_errors = FALSE,
                                 theme_color = "#008EA0FF",
                                 point_size = 1.1) {

  if (!inherits(model, "glm")) {

    stop("Model should be of class glm")

  } else {

    if (!is.logical(discrete_edm)) {
      stop("Input for discrete_edm argument must be logical: TRUE or FALSE")
    }

    # Avoid adding these packages under Imports in Description file
    insight::check_if_installed(c("gridExtra","ggrepel"))

    # Calculate the residuals
    df <- model$model
    pearson.dispersion <- summary(model)$dispersion
    working.leverages <- hatvalues(model)
    df$.resid.pearson.std <- resid(model, type = "pearson") / sqrt(pearson.dispersion * (1 - working.leverages))
    df$.resid.deviance.std <- rstandard(model)
    df$.resid.deviance.stu <- rstudent(model)
    df$.resid.quantile.std <- statmod::qresid(model) / sqrt(1 - working.leverages)
    df$.resid.working <- resid(model, type = "working")
    df$.linear.predictors <- model$linear.predictors
    df$.fitted <- fitted(model)
    df$.fitted.link <- predict(model, type = "link") # uses variance-stabilizing transformation of fitted values
    df$.cooks.distance <- cooks.distance(model)
    df$.hat <- lm.influence(model)$hat
    df$.sigma <- lm.influence(model)$sigma
    df$.dffits <- dffits(model)
    df$outlier <- ifelse(abs(df$.resid.deviance.std) > 3 | abs(df$.resid.quantile.std) > 3, rownames(df), NA)

    influence_measures <- lm.influence(model)
    cooks_distance <- cooks.distance(model)
    df_fits <- dffits(model)

    nr <- nrow(df)
    npara <- length(model$terms)
    h <- 4 / nr # cooks distance threshold
    df$cooksD <- ifelse(cooks_distance > h, rownames(df), NA)

    y_value_std <- ifelse(isTRUE(discrete_edm), ".resid.quantile.std", ".resid.deviance.std")
    y_label_std <- ifelse(isTRUE(discrete_edm), "Standardized Quantile Residuals", "Standardized Deviance Residuals")

    y_value_stu <- ifelse(isTRUE(discrete_edm), ".resid.quantile.std", ".resid.deviance.stu")
    y_label_stu <- ifelse(isTRUE(discrete_edm), "Standardized Quantile Residuals", "Studentized Deviance Residuals")

    if(sum(is.na(df$.resid.deviance.std)) > 0){
      stop("Insufficient degrees of freedom, check your model. Can't obtain diagnostic plots.")
    }

    plot_list <- list()

    ############################
    ### Systematic Component ### ----------------------------------------------
    ############################

    # Residuals vs Fitted -----------------------------------------------------
    resid_fitted_base <-
      ggplot(data = df,
             aes_string(x = ".fitted.link", # Link horizontally spreads out the fitted values to detect trends easier
                        y = y_value_std)) +
      geom_point(size = point_size, shape = 1) +
      geom_smooth(fill = "#d9d9d9", se = standard_errors, color = theme_color, size = 1.1) +
      labs(title = "Residuals vs Fitted",
           x = "Fitted Values", y = y_label_std) +
      theme_bw_nogrid()

    if (sum(abs(df$.resid.deviance.std) > 3) == 0 | sum(abs(df$.resid.quantile.std) > 3) == 0) {
      resid_fitted <- resid_fitted_base
    } else {
      resid_fitted <-
        resid_fitted_base +
        geom_point(size = point_size, shape = 1,
                   color = ifelse(abs(df$.resid.deviance.std) > 3, theme_color, "black")) +
        ggrepel::geom_label_repel(data = df,
                                  aes_string(label = "outlier"), na.rm = TRUE,
                                  max.overlaps = 20, color = "#5A9599FF")
    }


    # Link function check: Working Responses vs Linear Predictors -------------
    df$.working.linear <- df$.resid.working + df$.linear.predictors

    wresid_linear <-
      ggplot(data = df,
             aes_string(x = ".linear.predictors",
                        y = ".working.linear")) +
      geom_abline(intercept = 0, slope = 1, color = theme_color, size = 1.1) +
      geom_point(size = point_size, shape = 1) +
      labs(title = "Working Responses vs Linear Predictors",
           x = "Linear Predictors", y = "Working Responses") +
      theme_bw_nogrid()


    ########################
    ### Random Component ### --------------------------------------------------
    ########################

    # Q-Q plot with Quantile residuals since they have an exact Normal distribution
    slope <- (quantile(df$.resid.quantile.std, .75) - quantile(df$.resid.quantile.std, .25)) / (qnorm(.75) - qnorm(.25))
    intercept <- quantile(df$.resid.quantile.std, .25) - slope * qnorm(.25)
    qq_line <- data.frame(intercept = intercept, slope = slope)

    qq_res <-  df$.resid.quantile.std

    qq_plot <-
      ggplot(data = model) +
      stat_qq(aes_string(sample = "qq_res"), size = point_size, shape = 1) +
      geom_abline(data = qq_line,
                  aes(intercept = intercept, slope = slope),
                  color = theme_color, size = 1.1) +
      labs(title = "Normal Q-Q Plot",
           x = "Theoretical Quantile", y = "Standardized Quantile Residuals") +
      theme_bw_nogrid()

    ##############################
    ### Outliers & Influential ### --------------------------------------------
    ##############################

    # Detect outliers with studentized residuals > 3
    df$outlier.detect <- ifelse(abs(df$.resid.deviance.std) > 3 | abs(df$.resid.quantile.std) > 3,
                        rownames(df), NA)

    outlier_student <-
      ggplot(data = df,
             aes_string(x = 1:nr,
                        y = y_value_stu,
                        ymin = 0, ymax = max(y_value_stu))) +
      geom_linerange(color = "#bfbfbf") +
      geom_point(size = point_size, shape = 20) +
      geom_hline(yintercept = 3, color = theme_color, linetype = 2) +
      labs(title = "Outlier Detection",
           x = "Observation", y = y_label_stu) +
      theme_bw_nogrid() +
      ggrepel::geom_label_repel(data = df,
                                aes_string(label = "outlier.detect"), na.rm = TRUE,
                                max.overlaps = 20,
                                color = "#5A9599FF")

    # Detect outliers with halfnorm plot --------------------------------------
    halfnorm_hat <-
      gghalfnorm::gghalfnorm(influence_measures$hat, nlab = 5, repel = TRUE,
                             box.padding = ggplot2::unit(1, "lines"), color = "#008EA0FF") +
      labs(title = "Half Norm Plot using Leverage") +
      theme_bw_nogrid()


    halfnorm_cooks <-
      gghalfnorm::gghalfnorm(cooks_distance, nlab = 5, repel = TRUE, color = "#008EA0FF",
                             box.padding = ggplot2::unit(.5, "lines"), max.overlaps = 20) +
      labs(title = "Half Norm Plot using Cook's Distance") +
      theme_bw_nogrid()

    # Cooks distance plot -----------------------------------------------------
    limit <- max(cooks_distance, na.rm = T)
    margin_factor <- 5
    margin <- round(limit / margin_factor)
    max_cook <- limit + margin

    .cooks.distance <- NULL

    df$cook.detect <- ifelse(df$.cooks.distance > h, rownames(df), NA)

    cooks_distance_plot <-
      ggplot(data = df,
             aes(x = 1:nr, y = .cooks.distance, ymin = 0, ymax = cooks_distance)) +
      geom_linerange(color = "#bfbfbf") +
      geom_point(size = point_size, shape = 20) +
      geom_hline(yintercept = h, color = theme_color, linetype = 2) +
      labs(title = "Cook's Distance",
           x = "Observation", y = "Cook's Distance") +
      theme_bw_nogrid() +
      ggrepel::geom_label_repel(data = df, aes_string(label = "cook.detect"), na.rm = TRUE,
                                max.overlaps = 20, color = "#5A9599FF")

    # DFFITS plot -------------------------------------------------------------
    j <- 2 * sqrt((npara + 1) / (nr - npara - 1)) # dffits threshold
    df$dffits <- ifelse(abs(df_fits) > j, rownames(df), NA)

    dffits_plot <-
      ggplot(data = df,
             aes(x = 1:nr, y = .dffits, ymin = 0, ymax = df_fits)) +
      geom_linerange(color = "#bfbfbf") +
      geom_point(size = point_size, shape = 20) +
      geom_hline(yintercept = j, color = theme_color, linetype = 2) +
      geom_hline(yintercept = -j, color = theme_color, linetype = 2) +
      labs(title = "Difference in Fits",
           x = "Observation", y = "DFFITS") +
      theme_bw_nogrid() +
      ggrepel::geom_label_repel(data = df, aes_string(label = "dffits"), na.rm = TRUE,
                                max.overlaps = 20, color = "#5A9599FF")

    # Variation inflation factor plot -----------------------------------------
    # Same function as in `diagnostic_plots.R`
    vif_plot <- function(model,point_size = point_size, theme_color = theme_color) {
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

      results <- data.frame(Variables = vars, VIF = vifs)

      results$high_VIF <- ifelse(results$VIF  >= 5, results$Variables, NA)

      VIF <- NULL
      Variables <- NULL

      plt <-
        ggplot(results) +
        aes(x = Variables, y = VIF, ymin = 0, ymax = VIF) +
        geom_point(shape = 1, size = point_size, colour = theme_color) +
        geom_linerange(color = "#bfbfbf") +
        geom_hline(yintercept = 4, color = theme_color, linetype = 2) +
        geom_hline(yintercept = 10, color = theme_color, linetype = 2) +
        ggrepel::geom_label_repel(data = results, aes_string(label = "high_VIF"),
                                  na.rm = TRUE,
                                  max.overlaps = 20,
                                  color = theme_color) +
        labs(title = "VIF Plot (Collinearity)", x = "", y = "Variance Inflation Factor (VIF)") +
        theme_bw_nogrid()

      return(plt)
    }

    ####################
    ### Return Plots ### ------------------------------------------------------
    ####################

    # Return plots
    plot_list[[1]] <- resid_fitted
    plot_list[[2]] <- wresid_linear
    plot_list[[3]] <- qq_plot
    plot_list[[4]] <- outlier_student
    plot_list[[5]] <- halfnorm_hat
    plot_list[[6]] <- halfnorm_cooks
    plot_list[[7]] <- cooks_distance_plot
    plot_list[[8]] <- dffits_plot
    plot_list[[9]] <- vif_plot(model, point_size = point_size, theme_color = theme_color)

    names(plot_list) <- c("rf", "wp", "qq", "od_s", "od_h", "od_c", "cd", "df", "vi")

    return(suppressMessages(gridExtra::grid.arrange(grobs = plot_list[which_plots], ncol = n_columns)))
  }
}
