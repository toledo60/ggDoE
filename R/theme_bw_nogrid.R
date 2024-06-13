#' Theme for plots used in 'ggDoE'
#'
#' @importFrom ggplot2 %+replace% theme_bw element_blank element_text margin
#' @export
#' @return A simple black and white theme without grid.major and grid.minor for ggplot objects.
#'
#' @examples
#' library(ggplot2)
#' data <- ToothGrowth
#' data$dose <- factor(data$dose,levels = c(0.5, 1, 2),
#'                     labels = c("D0.5", "D1", "D2"))
#'
#' ggplot(data, aes(x=dose, y=len)) +
#'  geom_boxplot()+
#'  theme_bw_nogrid()
theme_bw_nogrid <- function(){

  theme_bw(base_size = 11,base_family = "") %+replace%    #replace elements we want to change
    theme(
      plot.title = element_text(
        size = 13,
        face = "plain",
        margin = margin(0, 0, 10, 0)
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
