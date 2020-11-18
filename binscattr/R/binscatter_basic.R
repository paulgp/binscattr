#' Basic Bin-scatter function
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

binscatter_basic <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE, connectdots = FALSE,
                       theme=theme_binscatter, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {
  x_label = enquo(x)
  y_label = enquo(y)

  if(length(controls) == 0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label)  , "|" ,
                               paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep="",collapse=" + "), sep=" "))
  }
  if(length(controls)!=0) {
    formula = as.formula(paste(quo_name(y_label), "~", quo_name(x_label), "+", paste(controls,sep="",collapse=" + ") , "|" ,
                               paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                               paste(clustervars,sep="",collapse=" + "), sep=" "))

    y_res_formula = as.formula(paste(quo_name(y_label), "~", paste(controls,sep="",collapse=" + ") , "|" ,
                                     paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep="",collapse=" + "), sep=" "))
    x_res_formula = as.formula(paste(quo_name(x_label), "~", paste(controls,sep="",collapse=" + ") , "|" ,
                                     paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                                     paste(c("0"),sep="",collapse=" + "), sep=" "))
    controls <- data[,controls]
  }

  x <- data[[quo_name(x_label)]]
  y <- data[[quo_name(y_label)]]

  f <- lfe::felm(formula, data=data)
  print(tidy(f)[2,3])
  beta <- paste("beta", formatC(tidy(f)[2,2][[1]], digits=3,format="fg", flag="#"), sep="=")
  se <-   paste("s.e.", formatC(tidy(f)[2,3][[1]], digits=3,format="fg", flag="#"), sep="=")

  if(length(controls) == 0) {
    data$x_binning <- x
    data$y_binning <- y
  } else {
    f_Xres <- lfe::felm(x_res_formula, data=data)
    f_Yres <- lfe::felm(y_res_formula, data=data)
    data$x_binning <- f_Xres$residuals + mean(x)
    data$y_binning <- f_Yres$residuals + mean(y)
  }

  g <- ggplot2::ggplot(data, aes(x = x_binning , y= y_binning))  + theme() +
    xlab(x_label) + ylab(y_label)
  if (scatter == TRUE) {
    g <- g + geom_point()
  }
  if (discrete == TRUE) {
    g <- g + stat_summary(fun = "mean",  colour = "#0072B2", size = 2.5, geom="point")
    if(connectdots == TRUE){
      g <- g + stat_summary(fun = "mean",  colour = "#0072B2", size = 1, geom="line")
    }
  }
  else {
    g <- g + stat_summary_bin(fun = "mean",  colour = "#0072B2", size = 2.5, geom="point", bins=20)
    if(connectdots == TRUE){
      g <- g + stat_summary_bin(fun = "mean",  colour = "#0072B2", size = 1, geom="line", bins=20)
    }
  }
  if (fitline == TRUE) {
    g <- g + geom_smooth(method='lm',formula=y~x, se=FALSE, color="#D55E00", size=1)
    posx <- c(Inf, Inf, -Inf, -Inf)
    posy <- c(Inf, -Inf, Inf, -Inf)
    posname <- c("top right", "bottom right", "top left", "bottom left")
    adjh <- c(1,1,-1,-1)
    adjv <- c(1,-1,1,-1)
    posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)

    # print(posdf)
    g <- g +
      geom_text(data = data.frame(x=Inf, y=-Inf), map = aes(x=x, y=y,hjust=1, vjust=-2.5, family = "Times New Roman"), label=beta) +
      geom_text(data = data.frame(x=Inf, y=-Inf), map = aes(x=x, y=y,hjust=1, vjust=-1, family = "Times New Roman"), label=se)
  }

  return(g)
}
