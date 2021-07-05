# Compare Model Coefficients ---------------------------------------------

#' Compare factor effects from multiple models and visualize
#' the difference in absolute value T statistics to check for model performances
#'
#' @param models a list of models to compare
#' @param model.names a vector of names for the models in format of "model_name"
#' @param showplot logical indicating to show the plot. If false, only the data used to construct the plot is returned. Default is TRUE
#'
#' @return summary output between models, and ggplot to compare models' consistency
#' @importFrom ggplot2 scale_x_discrete aes_string geom_point geom_line labs coord_flip
#' @importFrom stats coefficients
#' @importFrom dplyr bind_rows bind_cols as_tibble
#' @importFrom tidyr gather
#' @export
#'
#' @examples m1 <- lm(ybar~(A+B+C)^2,data =adapted_epitaxial)
#' m2 <- lm(ybar~(A+B)^2,data =adapted_epitaxial)
#'
#' models <- list(m1,m2)
#' models.names <- c("Model1","Model2")
#' summs_models(models,models.names)
summs_models <- function(models,model.names,showplot=TRUE) {
  if(showplot){
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


    dat_ungather = cbind("Factors"=attr(gg.dat,"dimnames")[[1]],
                         as.data.frame(gg.dat))
    dat1 = gather(dat_ungather,Model1,Model2,-Factors)
    colnames(dat1) = c("Factors","Model","absolute_T_stat")

    gg.plot = ggplot(dat1,aes_string(x='Factors',y='absolute_T_stat',
                                     group='Factors')) +
      theme_minimal() +
      geom_line(col="gray80",size=0.5)+
      geom_point(aes_string(x = 'Factors', y = 'absolute_T_stat',
                            color = 'Model'),size=4)+
      coord_flip()+
      scale_x_discrete(limits = rev(Factors))+
      labs(color="",x="",y="|t-statistic|")+
      theme(legend.position = "top")
    return(gg.plot)
  }
  else{
    t_dat <- bind_cols("Factors"=attr(gg.dat,"dimnames")[[1]],
                       as_tibble(gg.dat))
    return(t_dat)
  }
}
