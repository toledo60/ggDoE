library(tidyverse)


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
      melted_cormat <- reshape2::melt(abs(cor(M[,-1])))
    }
    else{
      melted_cormat <- reshape2::melt(abs(Alias_mat))
    }

    colors <- switch(palette,
                     "viridis" = viridisLite:::viridis(3,alpha = alpha,
                                                       direction = direction),
                     "cividis" = viridisLite:::viridis(3,alpha = alpha,
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

    plt <- ggplot(data = melted_cormat, aes(Var1, Var2, fill = value))+
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



design <- tibble::tribble(~X1, ~X2,~X3, ~X4, ~X5, ~X6,
                          1,1, -1,1,1,1,
                          -1, -1, 1, -1, -1, -1,
                          1,1,1, -1, 1, -1,
                          -1, -1, -1, 1, -1, 1,
                          1,1, -1, 1, -1, -1,
                          -1, -1, 1, -1,1,1,
                          1, -1, -1,1,1, -1,
                          -1,1,1, -1, -1, 1,
                          1, -1,1,1, -1, 1,
                          -1, 1, -1, -1, 1, -1,
                          1, -1, -1, -1, -1, 1,
                          -1,1,1,1,1, -1)


design2 <- tribble(~A, ~B, ~C, ~D, ~E,
                   -1, 1, -1, 1, 1,
                   -1,1,1,1, -1,
                   -1, -1, 1, -1, 1,
                   1, 1, 1, 1, 1,
                   1,1,1, -1, -1,
                   1, -1, -1, 1, -1,
                   -1, 1, -1, -1, 1,
                   1, -1, 1, 1, 1,
                   1, 1, -1, -1, -1,
                   -1, -1, 1, -1, -1,
                   1, -1, -1, -1, 1,
                   -1, -1, -1, 1, -1)


design3 <- tribble(~A,~B,~C,~D,~E,
                   -1, -1,1,1, -1,
                   -1, -1, -1, -1, -1,
                   1, -1, -1, 1, -1,
                   -1, 1, -1, -1, 1,
                   1, -1, -1, -1, 1,
                   1, -1, 1, 1, 1,
                   -1, 1, -1, 1, -1,
                   -1, -1, -1, 1, 1,
                   -1, -1, 1, -1, 1,
                   -1, 1, 1, 1, 1,
                   1,1,1, -1, -1,
                   1, 1, -1, 1, 1)

alias_matrix(design = design2)

