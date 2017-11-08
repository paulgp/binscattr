#' Binscatter Theme
#'
#' A nice ggplot theme for the bin scatter plots
#' @keywords binscatter theme
#' @export
#' @examples

theme_binscatter <- function () {
  theme_bw(base_size=14) %+replace%
    theme(
      legend.position = "bottom",
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border= element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text=element_text(size=14),
      axis.title=element_text(size=14))
}
