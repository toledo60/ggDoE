#' Obtain main effect plots in a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param n_columns number of columns for facet grid. Default is 2
#' @param exclude_vars A vector containing variables to exclude
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param showplot logical indicating to show the main effect plots. If false, a list of data.frames is returned used to obtain the main effects for each factor. Default is TRUE
#' @param ... additional parameters to be given to viridisPalette, such as alpha and direction
#' @return Main effects plots, or a list of tibble with calculated main effects for each factors if showplot=FALSE.
#' @export
#' @importFrom ggplot2 aes sym geom_point geom_line theme_bw labs facet_wrap scale_color_manual vars ylim element_blank
#' @importFrom utils stack
#' @importFrom stats aggregate setNames
#'
#' @examples
#' main_effects(original_epitaxial,response='s2',exclude_vars = c('ybar','lns2'))
#' main_effects(original_epitaxial,response='ybar',exclude_vars=c('A','s2','lns2'),n_columns=3)
main_effects <- function(design,response,
                         exclude_vars=c(),
                         n_columns=2,
                         color_palette = NA,
                         showplot=TRUE,
                         ...){
  if(inherits(design,'design')){
    design <- design_to_tibble(design,factors_to_numeric = TRUE)
  }

  factor_names <- setdiff(names(design), c(response, exclude_vars,'Blocks'))
  factors_total <- length(factor_names)

  dat_list <- lapply(factor_names, function(factor_name) {
    aggregated_data <- aggregate(design[[response]], list(design[[factor_name]]), mean)
    colnames(aggregated_data) <- c(factor_name, response)
    aggregated_data
  })
  names(dat_list) <- factor_names

  if(!showplot){
    return(dat_list)
  }
  else{
    vals <- sapply(dat_list, function(df) df[[2]])

    minval <- min(vals, na.rm = TRUE)
    maxval <- max(vals, na.rm = TRUE)

    basic_rbindlist <- function(data) {
      all_names <- unique(unlist(lapply(data, names)))
      filled_data <- lapply(data, function(df) {
        missing_names <- setdiff(all_names, names(df))
        if (length(missing_names) > 0) {
          missing_data <- setNames(rep(list(NA), length(missing_names)), missing_names)
          cbind(df, missing_data)
        } else {
          df
        }
      })
      return(do.call(rbind, filled_data))
    }

    dat <- basic_rbindlist(dat_list)

    dat[is.na(dat)] <- 888
    melt_dat <- stack(as.vector(dat))

    melted_dat <- melt_dat[melt_dat$values != 888 & melt_dat$ind != response,]
    melted_dat$response_var <- as.vector(dat)[[response]]
    melted_dat$values <- as.factor(melted_dat$values)

    if(is.na(color_palette)){
      factor_colors <- rep("#21908CFF",factors_total)
    }
    else{
      factor_colors <- viridisPalette(factors_total,
                                      color_palette = color_palette,
                                      ...)
    }

    p <- ggplot(melted_dat) +
      aes(x = !!sym('values'), y = !!sym('response_var'),
          colour = !!sym('ind'), group=1) +
      geom_line(aes(group=1),linewidth =0.5) +
      geom_point(size=1.5)+
      theme_bw() +
      ylim(minval,maxval)+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_color_manual(values = factor_colors)+
      facet_wrap(vars(ind),ncol = n_columns,scales = 'free_x')+
      labs(y=paste0("Mean of ",response),x='')

    return(p)
  }
}
