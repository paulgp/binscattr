#' Bin-scatter function
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
#' @keywords binscatter
#' @export
#' @examples
#' @importFrom lfe felm
#' @import dplyr
#' @import ggplot2
#' @import broom

binscatter <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE, grouping_var = c(),
                       theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {
  x_label = enquo(x)
  y_label = enquo(y)
  grouping_var = enquo(grouping_var)
  print(quo_name(grouping_var))
  print(quo_name(grouping_var) %in% colnames(data))

  if( quo_name(grouping_var) %in% colnames(data) ){
    print("Grouped Bin-Scatter")
    g <- binscatter_by_group(data, !!y_label, !!x_label, bins=20, discrete=FALSE, scatter=FALSE, grouping_var = !!grouping_var,
                             theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                             clustervars=c("0"), pos="bottom right")
  }
  else {
    print("Basic (Pooled) Bin Scatter")
    g <- binscatter_basic(data, !!y_label, !!x_label, bins=20, discrete=FALSE, scatter=FALSE,
                          theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                          clustervars=c("0"), pos="bottom right")
  }

  return(g)
}
