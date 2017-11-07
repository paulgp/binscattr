

library(ggplot2)
library(datasets)
library(lfe)
library(dplyr)
library(broom)

themePaul <- function () {
    theme_bw(base_size=12) %+replace%
        theme(
            legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border= element_blank(),     
            axis.line = element_line(colour = "black"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=10))
}

binscatter <- function(data, y, x, bins=20, discrete=FALSE, scatter=FALSE,
                       theme=themePaul, fitline=TRUE, controls=c(), absorb=c("0"),
                       clustervars=c("0"), pos="bottom right") {
    if(length(controls) == 0) {
        formula = as.formula(paste(y, "~", x  , "|" ,
            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
            paste(clustervars,sep="",collapse=" + "), sep=" "))
    }
    if(length(controls)!=0) {
        formula = as.formula(paste(y, "~", x, "+", paste(controls,sep="",collapse=" + ") , "|" ,
                        paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(clustervars,sep="",collapse=" + "), sep=" "))

        y_res_formula = as.formula(paste(y, "~", paste(controls,sep="",collapse=" + ") , "|" ,
                            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(c("0"),sep="",collapse=" + "), sep=" "))
        x_res_formula = as.formula(paste(x, "~", paste(controls,sep="",collapse=" + ") , "|" ,
                            paste(absorb,sep="",collapse=" + ") , "|" , "0" ,  "|" ,
                            paste(c("0"),sep="",collapse=" + "), sep=" "))
        controls <- data[,controls] 
    }   
    x_label = x
    y_label = y
    x <- data[,x]
    y <- data[,y]
    f <- felm(formula, data=data)
    print(tidy(f)[2,2:3])
    beta <- paste("beta", formatC(tidy(f)[2,2], digits=3,format="fg", flag="#"), sep="=")
    se <-   paste("s.e.", formatC(tidy(f)[2,3], digits=3,format="fg", flag="#"), sep="=")
    if(length(controls) != 0) { 
        f_Xres <- felm(x_res_formula, data=data)
        f_Yres <- felm(y_res_formula, data=data)
        x <- f_Xres$residuals + mean(x)
        y <- f_Yres$residuals + mean(y)
    }
    g <- ggplot(data, aes(x = x , y= y))  + themePaul() +
        xlab(x_label) + ylab(y_label)
    if (scatter == TRUE) {
        g <- g + geom_point() 
    }
    if (discrete == TRUE) {        
        g <- g + stat_summary(fun.y = "mean",  colour = "#0072B2", size = 2, geom="point")
    }
    else {
        g <- g + stat_summary_bin(fun.y = "mean",  colour = "#0072B2", size = 2, geom="point", bins=20)
    }
    if (fitline == TRUE) {
        g <- g + geom_smooth(method='lm',formula=y~x, se=FALSE, color="#D55E00", size=1)
        posx <- c(Inf, Inf, -Inf, -Inf)
        posy <- c(Inf, -Inf, Inf, -Inf)
        posname <- c("top right", "bottom right", "top left", "bottom left")
        adjh <- c(1,1,-1,-1)
        adjv <- c(1,-1,1,-1)
        posdf <- data.frame(posx, posy, adjh, adjv, row.names=posname)
        
        print(posdf)
        g <- g +
            geom_text(aes(x=Inf, y=-Inf,hjust=1, vjust=-2.5, label=beta)) +
            geom_text(aes(x=Inf, y=-Inf,hjust=1, vjust=-1, label=se))       
    }
    return(g)
}

data <- airquality %>% na.omit
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE)

