#' @title Second Order Model Design Matrix
#'
#' @description Obtain second order design matrix from a specified model. Optionally, be
#' able to extract specific terms from the matrix (i.e linear terms, quadratic terms, or bilinear terms)
#' @param model Input a second order model
#' @param terms specify if you only want to extract certain terms from the second order design matrix
#'
#' @return A design matrix from initial second order model
#' @export
#'
#' @examples model2 <- lm(ybar ~ (A+B)^2 + I(A^2)+I(B^2), data= epitaxial)
#' second_order_mat(model2)
#' second_order_mat(model2, terms = "linear")
#' second_order_mat(model2, terms = "quadratic")
#'
second_order_mat <- function(model,terms="full") {

  k = ncol(model$model[,-which(sapply(model$model, class) == "AsIs")])-1
  b = length(which(attr(model$terms,"order")==2)) # bilinear terms
  q = ncol(model$model[,which(sapply(model$model, class) == "AsIs")])   # quad terms in model
  p = dim(model.matrix(model))[2] # parameters in model


  I = attr(model.matrix(model),"dimnames")[[2]][1]
  L = attr(model.matrix(model),"dimnames")[[2]][2:(k+1)]
  Q = attr(model.matrix(model),"dimnames")[[2]][(k+2):(k+(q+1))]
  B = attr(model.matrix(model),"dimnames")[[2]][(k+(q+2)):p]


  M = model.matrix(model)
  order = c(I,Q,L,B)


  if (terms=="full"){
    return(M[,order])
  }
  else if (terms =="linear"){
    return(M[,L])
  }
  else if (terms == "bilinear"){
    return(M[,B])
  }
  else if (terms == "quadratic"){
    return(M[,Q])
  }
  else {
    stop("available terms: full, linear, bilinear, quadratic")
  }
}