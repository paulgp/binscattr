#' Bin-scatter by group function
#'
#' This function does a binscatter for two-dimensional data
#' @param data Dataframe containing the x and y variables
#' @param y The independent variable y
#' @param x The dependent variable x
#' @param bins The number of bins
#' @param discrete Discrete?
#' @param scatter Not sure what this is yet...
#' @param theme Optional ggplot theme
#' @param fitline Option to add a linear fit through the bin scatter
#' @param controls Optional control variables to the binscatter
#' @param clustervars Optional cluster variables
#' @param group Grouping variable
#' @keywords binscatter
#' @export
#' @examples
#' @importFrom lfe felm
#' @import dplyr
#' @import ggplot2
#' @import broom

binscatter_by_group <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE, group_by,
                       theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {

  group_by = enquo(group_by)
  x_label = enquo(x)
  y_label = enquo(y)

  data <- data %>%
    group_by(!!group_by) %>%
    do(
      get_binning_residuals(., !!y_label, !!x_label, controls, absorb, clustervars)
    )

    data <- data %>%
      mutate(
        group_factor = as.character(!!group_by)
      )

  g <- ggplot2::ggplot(data, aes(x = x_group_binning , y = y_group_binning, color = group_factor) )  +
    theme() +
    xlab(x_label) +
    ylab(y_label)

  if (scatter == TRUE) {
    g <- g + geom_point(aes( color = factor(!!group_by )))
  }
  if (discrete == TRUE) {
    g <- g + stat_summary(fun.y = "mean", size = 2, geom="point")
  }
  else {
    g <- g + stat_summary_bin(fun.y = "mean", size = 2, geom="point", bins=20)
  }
  if (fitline == TRUE) {
    g <- g + geom_smooth(method='lm',formula=y~x, se=FALSE, size=1)
    posx <- c(Inf, Inf, -Inf, -Inf)
    posy <- c(Inf, -Inf, Inf, -Inf)
    posname <- c("top right", "bottom right", "top left", "bottom left")
    adjh <- c(1,1,-1,-1)
    adjv <- c(1,-1,1,-1)
    posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)
  }

  return(g)
}
