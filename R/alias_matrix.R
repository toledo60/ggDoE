#' Color Map on Correlations
#'
#' @param design Design of experiment (Without response)
#' @param midpoint A midpoint value between (0,1) to split the color scheme of three colors
#' @param digits number of digits to round correlation values. Default is 3
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @param showplot logical indicating to show the correlation plot. If false, the correlation/alias matrix is returned. Default is TRUE
#' @importFrom ggplot2 scale_fill_gradient2 geom_tile element_blank aes_string theme_minimal theme
#' @importFrom stats model.matrix cor
#' @return correlation matrix between main effects and interaction effects from the model.matrix. Alias matrix is also returned
#' @export
#'
#' @examples
#' alias_matrix(design=aliased_design)
#' alias_matrix(design=aliased_design, color_palette = "plasma")
#' alias_matrix(design=aliased_design, color_palette = "magma", direction = -1)
alias_matrix <- function(design,midpoint=0.5,
                          digits=3,
                          color_palette = "viridis",
                          alpha=1,direction = 1,
                          showplot=TRUE){

  k <- ncol(design)   # k is number of input factors


  M <- as.matrix(model.matrix(~ (.)^2, data = design))


  bilinear_terms <- as.matrix(M[, (k+2):ncol(M)])
  linear_terms <- as.matrix(M[,!(colnames(M) %in% colnames(bilinear_terms))])

  A <- solve(t(linear_terms) %*% linear_terms) %*% t(linear_terms)%*%(bilinear_terms)

  Alias_mat <- round(A,digits = digits)[-1,]

  if(showplot){

    dat <- abs(cor(M[,-1]))
    melted_cormat <- cbind(expand.grid(colnames(dat),rownames(dat)),
                           'value'= as.vector(dat))

    colors <- viridisPalette(3,
                             color_palette = color_palette,
                             direction = direction,
                             alpha = alpha)

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
