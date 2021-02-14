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










# OACD Table Cross Validation ---------------------------------------------



#' Compare factor effects from multiple models and visualize
#' the difference in absolute value T statistics to check for model performances
#'
#' @param models a list of models to compare
#' @param model.names a vector of names for the models in format of "model1"
#'
#' @return summary output between models, and ggplot to compare models consistency
#' @export
#'
#' @examples m1 <- lm(ybar~(A+B+C)^2,data =epitaxial)
#' m2 <- lm(ybar~(A+B)^2,data =epitaxial)
#'
#' models <- list(m1,m2)
#' models.names <- c("Model1","Model2")
#' summs_models(models,models.names)
summs_models <- function(models,model.names) {


  coefficients = unlist(lapply(models,coefficients))
  coeff.names = unique(names(coefficients))[-1]

  models.data = list()

  for (i in 1:length(models)) {
    models.data[[i]] = as.data.frame(t(abs(summary(models[[i]])$coefficients[-1,3])))

    gg.dat = t(bind_rows(models.data))
    gg.dat[is.na(gg.dat)] = 0
  }
  colnames(gg.dat) = model.names
  Factors = factor(coeff.names,levels = coeff.names)

  summ =  jtools::export_summs(models,
                               error_format = '',
                               coefs  = coeff.names,model.names =model.names,
                               stars = c(`'`=0.1,`*` = 0.05, `**` = 0.01, `***` = 0.001),
                               digits = 4)

  dat1 = melt(gg.dat)
  colnames(dat1) = c("Factors","Model","absolute_T_stat")

  gg.plot = ggplot(dat1) +
    theme_minimal() +
    geom_line(aes(x=Factors,y=absolute_T_stat,group=Factors),col="gray80",size=0.5)+
    geom_point(aes(x = Factors, y = absolute_T_stat, color = Model),size=4)+
    coord_flip()+
    scale_x_discrete(limits = rev(unique(sort(dat1$Factors))))+
    labs(color="",x="",y="|t-statistic|")+
    theme(legend.position = "top")


  p = c() # parameters
  N = c() # Design Runs
  k = c() # Levels in each design
  D_efficiencies = c()

  for (i in 1:length(models)) {
    p[i] = dim( model.matrix( models[[i]] ) )[2]
    N[i] = dim( model.matrix( models[[i]] ))[1]

    if(sum(sapply(models[[i]]$model, class) == "AsIs")!=0){

      k[i]= ncol(models[[i]]$model[,-which(sapply(models[[i]]$model, class) == "AsIs")])-1
    }
    else{

      k[i]=ncol(models[[i]]$model)-1
    }


    D_efficiencies[i]=
      ((det(t(model.matrix(models[[i]]))%*%model.matrix(models[[i]])))^(1/p[i]))/N[i]
  }

  D_eff_table = data.frame(model.names,k,D_efficiencies)


  return(list(table=summ,t_dat=gg.dat,plot=gg.plot,D_effs=D_eff_table))
}






