#' Color Map on Correlations
#'
#' @param design Design of experiment (Without response)
#' @param midpoint A midpoint value between (0,1) to split the color scheme of three colors
#' @param digits number of digits to round correlation values. Default is 3
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param showplot logical indicating to show the correlation plot. If false, the correlation/alias matrix is returned. Default is TRUE
#' @param ... additional parameters to be given to viridisPalette, such as alpha and direction
#' @importFrom ggplot2 scale_fill_gradient2 geom_tile element_blank aes theme_minimal theme
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
                         showplot=TRUE,
                         ...){

  k <- ncol(design)   # k is number of input factors
  M <- as.matrix(model.matrix(~ (.)^2, data = design))

  bilinear_terms <- as.matrix(M[, (k+2):ncol(M)])
  linear_terms <- as.matrix(M[,!(colnames(M) %in% colnames(bilinear_terms))])

  A <- solve(t(linear_terms) %*% linear_terms) %*% t(linear_terms)%*%(bilinear_terms)
  Alias_mat <- round(A,digits = digits)[-1,]

  if(!showplot){
    return(Alias_mat)
  }
  else{
    dat <- abs(cor(M[,-1]))
    melted_cormat <- cbind.data.frame(expand.grid(colnames(dat),rownames(dat)),
                                      'value'= as.vector(dat))

    colors <- viridisPalette(3,
                             color_palette = color_palette,
                             ...)
    plt <- ggplot(data = melted_cormat,
                  aes(!!melted_cormat$Var1, !!melted_cormat$Var2, fill = !!melted_cormat$value))+
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
  }
}
