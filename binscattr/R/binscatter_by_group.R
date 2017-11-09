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

binscatter_by_group <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE, connectdots = FALSE,
                                grouping_var, theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {

  grouping_var = enquo(grouping_var)
  x_label = enquo(x)
  y_label = enquo(y)

  # Produce residualized variables by group
  data <- data %>%
    group_by(!!grouping_var) %>%
    do(
      get_binning_residuals(., !!y_label, !!x_label, controls, absorb, clustervars)
    )

  # Make sure the grouping variable is treated as a discrete object
  data <- data %>%
    mutate(
      group_factor = as.character(!!grouping_var)
    )

  g <- ggplot2::ggplot(data, aes(x = x_binning , y = y_binning, color = group_factor) )  +
    theme() +
    labs(
      x = x_label,
      y = y_label,
      color = quo_name(grouping_var)
    )


  if (scatter == TRUE) {
    g <- g + geom_point(aes( color = factor(!!grouping_var )))
  }
  if (discrete == TRUE) {
    g <- g + stat_summary(fun.y = "mean", size = 2.5, geom="point")
    if(connectdots == TRUE){
      g <- g + stat_summary(fun.y = "mean", size = 1, geom="line")
    }
  }
  else {
    g <- g + stat_summary_bin(fun.y = "mean", size = 2.5, geom = "point", bins = bins)
    if(connectdots == TRUE){
      g <- g + stat_summary_bin(fun.y = "mean",  size = 1, geom = "line", bins = bins)
    }
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
