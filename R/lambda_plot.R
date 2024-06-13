#' Lambda Plot: Trace of t-statistics
#'
#' @param model Model of class "lm" or "glm"
#' @param lambda sequence of lambda values to consider for plot. Default is seq(-2,2,0.1)
#' @param color_palette A character string indicating the color map option to use.
#' Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo". Default is 'viridis'
#' @param showplot logical indicating to show the main effect plots. If false, a list of tibbles is returned used to obtain the main effects for each factor. Default is TRUE
#' @param ... additional parameters to be given to viridisPalette, such as alpha and direction
#' @return Lambda plot for tracing t-staitics across different values of lambda (in ggplot2)
#' @export
#'
#' @importFrom stats lm as.formula
#' @importFrom utils stack
#' @importFrom ggplot2 aes scale_color_manual theme_bw element_blank labs
#' @examples
#' mod <- lm(s2 ~ (A+B+C)^2,data=original_epitaxial)
#' lambda_plot(mod)
#' lambda_plot(mod,lambda = seq(0,2,0.1))
#' lambda_plot(mod,lambda = seq(0,2,0.1),showplot = FALSE)
lambda_plot <- function(model, lambda = seq(-2, 2, by = 0.1),
                        color_palette = 'viridis',
                        showplot=TRUE,
                        ...){
  if(!insight::is_regression_model(model)){
    stop("model should be a regression model of class 'lm'")
  }
  insight::check_if_installed('ggrepel')

  y <- model$model[, 1]
  response <- model$terms[[2]]
  variables <- attr(model$terms,'term.labels')

  var_formula <- paste(variables,collapse = '+')
  data_name <- model$call[[3]]

  org_fit <- lm(as.formula(paste(response,"~",var_formula)), qr = TRUE,
                data = eval(data_name))

  QR <- org_fit$qr
  n <- length(y)
  p <- length(coef(org_fit))
  idx <- 1:p
  rdf <- n - p

  coef_lambda <- matrix(NA, nrow = p,
                        ncol = length(lambda))

  t_lambda <- se_lambda <- coef_lambda

  for (j in seq(lambda)) {
    l <- lambda[j]
    if (l == 0){
      y_lambda <- log(y)
    }
    else {
      y_lambda <- (y^l - 1)/l
    }
    resvar <- sum(qr.resid(QR, y_lambda)^2)/rdf
    coef_lambda[, j] <- qr.coef(QR, y_lambda)
    R <- chol2inv(QR$qr[idx, idx, drop = FALSE])
    se_lambda[, j] <- sqrt(diag(R) * resvar)
  }

  t_lambda <- t(coef_lambda/se_lambda)[,-1]
  colnames(t_lambda) <- names(coef(org_fit))[-1]

  t_lambda_dat <- tibble::tibble(lambda = lambda,
                                 tibble::as_tibble(t_lambda))

  if(!showplot){
    return(t_lambda_dat)
  }else{

    factors_total <- ncol(t_lambda)

    melted_t <- data.frame(cbind('lambda'=rep(lambda,factors_total),
                                 stack(as.vector(t_lambda_dat[,-1]))))

    pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
    int_terms <- variables[grepl(pattern,variables)]

    label_left <- melted_t[lambda == lambda[1],]
    label_left_main <- label_left[!label_left$ind %in% int_terms,]

    label_right <- melted_t[lambda == lambda[length(lambda)],]
    label_right_interactions <- label_right[label_right$ind %in% int_terms,]

    if(is.na(color_palette)){
      factor_colors <- rep("#21908CFF",factors_total)
    }else{
      factor_colors <- viridisPalette(factors_total,
                                      color_palette = color_palette,
                                      ...)
    }

    plt <- ggplot(melted_t, aes(x=!!sym('lambda'), y=!!sym("values"),
                                colour=!!sym("ind"),
                                group=!!sym("ind")))+
      geom_line()+
      ggrepel::geom_label_repel(data = label_left_main,aes(label=ind),
                                max.overlaps = 15)+
      ggrepel::geom_label_repel(data = label_right_interactions,
                                aes(label=ind),
                                max.overlaps = 15)+
      labs(x='lambda',y='t-statistic',title='Lambda Plot:',
           subtitle = 'Trace of t-statistic')+
      scale_color_manual(values = factor_colors)+
      theme_bw()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    return(plt)

  }
}
