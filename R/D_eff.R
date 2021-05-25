#' D-efficiency for Composite Designs Based on Orthogonal Arrays
#'
#' @param X Composite design matrix
#' @param design_type Specify between orthogonal array composite design (OACD) or central
#' composite design (CCD)
#' @param subset a subset of factors from our composite design
#' @param ncenter number of centers in our design
#' @param efficiency default is true,and returns D-efficiency. If false,
#'  it will return D-optimal criteria instead
#' @param lower_bounds lower bounds of OACD D-efficiency
#'
#' @return
#' @export
#'
#' @examples
D_eff<- function(X,design_type,subset=NA,ncenter = 0,efficiency=TRUE,
                 lower_bounds=FALSE)
{ if(is.null(design_type)){stop('specify design type: "OACD" or "CCD"')}
  ## expand design to include second order terms
  ## add columns for constant, X, X^2 and all X_i * X_j
  k <- ncol(X)   # k is number of input factors
  d <- as.data.frame(X)
  M <- model.matrix(~ (.)^2, data = d)
  bilinear <- M[, (k+2):ncol(M)]
  quad <- X^2
  result <- cbind(M[,1],quad, M[,2:(k+1)], bilinear)


  center_runs_matrix <- cbind(rep(1,ncenter),
                              matrix(0,ncol = ncol(result)-1,nrow=ncenter))

  Design <- as.matrix(rbind(result,center_runs_matrix))
  colnames(Design) <- NULL

  N = nrow(Design) # N= n1+n2+n0
  axial_points = tail(X,2*k)
  n2= nrow(axial_points)
  n1 = nrow(X)- n2 # number of runs in factorial design
  n0 =  ncenter
  q = k*(k-1)*0.5 # number of bilinear terms

  detM <- determinant(crossprod(Design) ,
                      logarithm = TRUE)$modulus
  attributes(detM) <- NULL

  # log(det(M(.))) for approximate D-optimal design

  u <- ((k+3) * ((2*k^2 + 3*k + 7) + (k-1) * sqrt(4*k^2 + 12*k + 17)) /
          (4 * (k+1) * (k+2)^2))
  v <- ((k+3) * ((4*k^3 + 8*k^2 + 11*k - 5) + (2*k^2 + k + 3) * sqrt(4*k^2 + 12 * k + 17)) /
          (8 * (k+2)^3 * (k+1)))

  logMopt <- k * log(u) + 0.5 * k * (k-1) * log(v) + (k-1) * log(u-v) + log(u + (k-1)*v - k*u^2)

  p <- (k+2)*(k+1)/2  # of parameters of design matrix

  if(!is.na(subset) && !is.na(design_type)){
    if(subset=="linear" && design_type=="OACD"){
      subLinear <- Design[,-c( (k+2):(1+2*k))]
      colnames(subLinear) <- NULL
      detS <- determinant(crossprod(subLinear),
                          logarithm = TRUE)$modulus
      attributes(detS) <- NULL
      s= k
      Ds <- (detM - detS) / s
      return(exp(Ds)/N)
    }
    else if(subset=="bilinear"  && design_type=="OACD"){
      subBilinear <- Design[,-c((2+2*k):ncol(Design) )]
      colnames(subBilinear) <- NULL
      detS <- determinant(crossprod(subBilinear),
                          logarithm = TRUE)$modulus
      attributes(detS) <- NULL
      s= k*(k-1)*0.5
      Ds <- (detM - detS) / s
      return(exp(Ds)/N)
    }
    else if(subset=="quadratic"  && design_type=="OACD"){
      subQuad <- Design[,-c(2:(k+1))]
      colnames(subQuad) <- NULL
      detS <- determinant(crossprod(subQuad),
                          logarithm = TRUE)$modulus
      attributes(detS) <- NULL
      s= k
      Ds <- (detM - detS) / s
      if (efficiency){
        return((4*exp(Ds))/N)
      }
      else{
        return(exp(Ds)/N)
      }
    }
    else if(subset=="linear"  && design_type=="CCD"){


      detS <- log((2^k*n1^q)*( (k-1)^2*n1 + (1+k*n1*0.5)*n0 ))
      s= k
      Ds <- (detM - detS) / s
      return(exp(Ds)/N)
    }
    else if(subset=="bilinear"  && design_type=="CCD"){

      detS <- log((2*n1+ 4)^k * ( (k-1)^2*n1 + (1+k*n1*0.5)*n0 ))
      s= k*(k-1)*0.5
      Ds <- (detM - detS) / s
      return(exp(Ds)/N)
    }
    else if(subset=="quadratic"  && design_type=="CCD"){

      q = k*(k-1)*0.5

      detS = log(N*n1^q*(n1+2)^k)
      s= k
      Ds <- (detM - detS) / s
      if (efficiency){
        return((4*exp(Ds))/N)
      }
      else{
        return(exp(Ds)/N)
      }

    }
  }

  if (efficiency && lower_bounds==FALSE){
    logDE <- (detM - logMopt) / p # D-efficiency
    return(exp(logDE)/N)
  }
  else if(!efficiency && lower_bounds==FALSE){
    logD <- (detM)/p # D-optimal
    return(exp(logD)/N)
  }

  if(lower_bounds && design_type =="OACD" && efficiency){
    D_Leff_lower <- (1/N) * ( (9*n1)/(9*n1 + 4*n2)   )^(q/k) * (n1 + (2/3)*n2)

    D_Beff_lower <- n1/N

    D_Qeff_lower <- ((8*n2)/ (9*N^((k+1)/k) ) ) *
      ((9*n1)/(9*n1 + 4*n2) )^(q/k)*((1+2*k)*n0 + n2 + n1 * (1 + 0.5*k + ((9*k*n0)/(2*n2))) )^(1/k)

    LB_oacd <- n1^q * (     ((6*n1 + 4*n2)*n2 )/27   )^k *
      ((1+2*k)*n0 + n2 + n1 * (1 + 0.5*k + ((9*k*n0)/(2*n2))) )

    Max_Dk = exp(logMopt)
    D_eff_lower = (1/N)* (LB_oacd/Max_Dk)^(1/p)

    return(tibble("D_eff_lower" = D_eff_lower,
                  "L_eff_lower" = D_Leff_lower,
                  "B_eff_lower" = D_Beff_lower,
                  "Q_eff_lower" = D_Qeff_lower))
  }else if (lower_bounds && (design_type !="OACD" || !efficiency) ){
    stop('D-efficiency lower bounds only supported for OACD, make sure to\n specify design_type ="OACD", efficiency=TRUE')
  }
}