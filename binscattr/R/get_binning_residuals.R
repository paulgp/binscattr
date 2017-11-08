#' Bin-scatter Residualizing function
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
#' @import broom

get_binning_residuals <- function(data, y, x, controls=c(), absorb=c("0"), clustervars=c("0")){
  x_label = enquo(x)
  y_label = enquo(y)

  if (length(controls) == 0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label)  , "|" ,
                               paste(absorb,sep = "",collapse = " + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep = "",collapse = " + "), sep = " "))
  }
  if (length(controls) != 0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label), "+", paste(controls,sep = "",collapse = " + ") , "|" ,
                               paste(absorb,sep = "",collapse = " + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep = "",collapse = " + "), sep = " "))

    y_res_formula = as.formula(paste(quo_name(y_label), "~", paste(controls,sep = "",collapse = " + ") , "|" ,
                                     paste(absorb,sep = "",collapse = " + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep = "",collapse = " + "), sep = " "))
    x_res_formula = as.formula(paste(quo_name(x_label), "~", paste(controls,sep = "",collapse = " + ") , "|" ,
                                     paste(absorb,sep = "",collapse = " + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep = "",collapse = " + "), sep = " "))
    controls <- data[,controls]
  }

  x <- data[[quo_name(x_label)]]
  y <- data[[quo_name(y_label)]]

  f <- lfe::felm(formula, data = data)
  print(tidy(f)[2,2:3])

  if (length(controls) == 0) {
    data$x_group_binning <- x
    data$y_group_binning <- y
  } else {
    f_Xres <- lfe::felm(x_res_formula, data = data)
    f_Yres <- lfe::felm(y_res_formula, data = data)
    data$x_group_binning <- f_Xres$residuals + mean(x)
    data$y_group_binning <- f_Yres$residuals + mean(y)
  }
  return(data)
}
