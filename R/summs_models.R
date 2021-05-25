# Table Cross Validation ---------------------------------------------



#' Compare factor effects from multiple models and visualize
#' the difference in absolute value T statistics to check for model performances
#'
#' @param models a list of models to compare
#' @param model.names a vector of names for the models in format of "model_name"
#'
#' @return summary output between models, and ggplot to compare models' consistency
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

  dat_ungather = cbind("Factors"=attr(gg.dat,"dimnames")[[1]],
                       as.data.frame(gg.dat))
  dat1=gather(dat_ungather,Model1,Model2,-Factors)
  colnames(dat1) = c("Factors","Model","absolute_T_stat")

  gg.plot = ggplot(dat1,aes(x=Factors,y=absolute_T_stat,group=Factors)) +
    theme_minimal() +
    geom_line(col="gray80",size=0.5)+
    geom_point(aes(x = Factors, y = absolute_T_stat, color = Model),size=4)+
    coord_flip()+
    scale_x_discrete(limits = rev(Factors))+
    labs(color="",x="",y="|t-statistic|")+
    theme(legend.position = "top")

  t_dat <- bind_cols("Factors"=attr(gg.dat,"dimnames")[[1]],
                     as_tibble(gg.dat))

  return(list(table=summ,t_dat= t_dat,plot=gg.plot))
}