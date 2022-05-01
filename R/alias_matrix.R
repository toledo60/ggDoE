#' Color Map on Correlations
#'
#' @param design Design of experiment (Without response)
#' @param midpoint A midpoint value between (0,1) to split the color scheme of three colors
#' @param intercept logical indicating to include the intercept coefficient. Default is TRUE
#' @param showplot logical indicating to show the correlation plot. If false, the correlation/alias matrix is returned. Default is TRUE
#' @param symmetric logical indicating to return all pairwise correlations between main effects and interaction effects. Default is FALSE
#' @param digits number of digits to round correlation values. Default is 3
#' @param palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @importFrom reshape2 melt
#' @importFrom ggplot2 scale_fill_gradient2 geom_tile element_blank aes_string theme_minimal theme
#' @importFrom stats model.matrix cor
#' @return correlation matrix between main effects and interaction effects from the model.matrix. Alias matrix is also returned
#' @export
#'
#' @examples
#' alias_matrix(design=aliased_design)
#' alias_matrix(design=aliased_design, symmetric=TRUE)
#' alias_matrix(design=aliased_design, symmetric=TRUE, palette = "plasma")
#' alias_matrix(design=aliased_design, symmetric = TRUE, palette = "magma", direction = -1)
alias_matrix <- function(design,midpoint=0.5,
                         intercept=TRUE,showplot=TRUE,
                         symmetric = FALSE,
                         digits=3,palette = "viridis",
                         alpha=1,direction = 1){

  k <- ncol(design)   # k is number of input factors


  M <- as.matrix(model.matrix(~ (.)^2, data = design))


  bilinear_terms <- as.matrix(M[, (k+2):ncol(M)])
  linear_terms <- as.matrix(M[,!(colnames(M) %in% colnames(bilinear_terms))])

  A <- solve(t(linear_terms) %*% linear_terms) %*% t(linear_terms)%*%(bilinear_terms)

  if(intercept){
    Alias_mat <- round(A,digits = digits)
  }
  else{
    Alias_mat <- round(A,digits = digits)[-1,]
  }

  if(showplot){
    if(symmetric){
      melted_cormat <- melt(abs(cor(M[,-1])))
    }
    else{
      melted_cormat <- melt(abs(Alias_mat))
    }

    colors <- switch(palette,
                     "viridis" = viridisLite::viridis(3,alpha = alpha,
                                                      direction = direction),
                     "cividis" = viridisLite::cividis(3,alpha = alpha,
                                                      direction = direction),
                     "magma" = viridisLite::magma(3,alpha = alpha,
                                                  direction = direction),
                     "inferno" = viridisLite::inferno(3,alpha = alpha,
                                                      direction = direction),
                     "plasma" = viridisLite::plasma(3,alpha = alpha,
                                                    direction = direction),
                     "rocket" = viridisLite::rocket(3,alpha = alpha,
                                                    direction = direction),
                     "mako" = viridisLite::mako(3,alpha = alpha,
                                                direction = direction),
                     "turbo" = viridisLite::turbo(3,alpha = alpha,
                                                  direction = direction))

    plt <- ggplot(data = melted_cormat, aes_string('Var1', 'Var2', fill = 'value'))+
      geom_tile(color = "#001526")+
      scale_fill_gradient2(low = colors[1], mid = colors[2],high = colors[3],
                           midpoint = midpoint, limit = c(0,1), space = "Lab",
                           name="Absolute\nCorrelation\n") +
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    return(plt)
  }else{
    return(Alias_mat)
  }
}
