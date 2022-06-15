#' Simple viridisLite wrapper
#'
#' @param total_colors number of colors desired
#' @param color_palette A character string indicating the color map option to use. Eight options are available: "viridis","cividis","magma","inferno","plasma","rocket","mako","turbo"
#' @param alpha The alpha transparency, a number in [0,1]
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed
#'
#' @return Specified color palette of length `total_colors`
#' @export
#'
#' @examples
#' viridisPalette(5)
#' viridisPalette(5,color_palette='magma',alpha=0.5)
#' viridisPalette(5,color_palette='plasma',alpha=0.6,direction=-1)
viridisPalette <- function(total_colors,color_palette = "viridis",
                           alpha=1,direction = 1){
  colors_palette <- switch(color_palette,
                           "viridis" = viridisLite::viridis(total_colors,
                                                            alpha = alpha,
                                                            direction = direction),
                           "cividis" = viridisLite::cividis(total_colors,
                                                            alpha = alpha,
                                                            direction = direction),
                           "magma" = viridisLite::magma(total_colors,
                                                        alpha = alpha,
                                                        direction = direction),
                           "inferno" = viridisLite::inferno(total_colors,
                                                            alpha = alpha,
                                                            direction = direction),
                           "plasma" = viridisLite::plasma(total_colors,
                                                          alpha = alpha,
                                                          direction = direction),
                           "rocket" = viridisLite::rocket(total_colors,
                                                          alpha = alpha,
                                                          direction = direction),
                           "mako" = viridisLite::mako(total_colors,
                                                      alpha = alpha,
                                                      direction = direction),
                           "turbo" = viridisLite::turbo(total_colors,
                                                        alpha = alpha,
                                                        direction = direction))
  return(colors_palette)
}
