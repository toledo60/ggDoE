#' Obtain main effect plots in a factorial design
#'
#' @param design Design of experiment (Factorial Design)
#' @param response A character string indicating the response of the data
#' @param n_columns number of columns for facet grid. Default is 2
#' @param exclude_vars A vector containing variables to exclude
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#' @param showplot logical indicating to show the main effect plots. If false, a list of data.frames is returned used to obtain the main effects for each factor. Default is TRUE
#'
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
                         alpha=1,direction = 1,
                         showplot=TRUE){

  factor_names <- setdiff(names(design), c(response, exclude_vars))
  factors_total <- length(factor_names)

  dat_list <- vector("list", length = factors_total)

  for (i in seq_along(factor_names)) {
    dat_list[[i]] <- aggregate(design[[response]],
                               design[factor_names[i]], mean)
    colnames(dat_list[[i]]) <- c(factor_names[i], response)
  }
  names(dat_list) <- factor_names

  if(!showplot){
    return(dat_list)
  }
  else{
    vals <- c()

    for (i in 1:factors_total) {
      vals <- c(vals, dat_list[[i]][[2]])
    }

    minval <- min(vals)
    maxval <- max(vals)

    basic_rbindlist <- function(data) {
      all_names <- unique(unlist(lapply(data, names)))
      filled_data <- vector("list", length = length(data))

      for (i in seq_along(data)) {
        this_names <- names(data[[i]])
        missing_names <- setdiff(all_names, this_names)
        # If any names are missing, add them to the data frame with NAs
        if (length(missing_names) > 0) {
          missing_data <- setNames(rep(list(NA), length(missing_names)),
                                   missing_names)
          filled_data[[i]] <- cbind(data[[i]], missing_data)
        } else {
          filled_data[[i]] <- data[[i]]
        }
      }
      return(do.call(rbind, filled_data))
    }

    dat <- basic_rbindlist(dat_list)

    dat[is.na(dat)] <- 88
    melt_dat <- stack(as.vector(dat))

    melted_dat <- melt_dat[melt_dat$values != 88 & melt_dat$ind != response,]
    melted_dat$response_var <- as.vector(dat)[[response]]
    melted_dat$values <- as.factor(melted_dat$values)

    if(is.na(color_palette)){
      factor_colors <- rep("#21908CFF",factors_total)
    }
    else{
      factor_colors <- viridisPalette(factors_total,
                                      color_palette = color_palette,
                                      direction = direction,
                                      alpha = alpha)
    }

    p <- ggplot(melted_dat) +
      aes(x = !!sym('values'), y = !!sym('response_var'),
          colour = !!sym('ind'), group=1) +
      geom_line(aes(group=1),linewidth =0.5) +
      geom_point(size=1)+
      theme_bw() +
      ylim(minval,maxval)+
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_color_manual(values = factor_colors)+
      facet_wrap(vars(ind),ncol = n_columns)+
      labs(y=paste0("Mean of ",response),x='')

    return(p)
  }
}
