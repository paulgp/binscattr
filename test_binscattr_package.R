library(datasets)
library(tidyverse)
library(binscattr)

data <- airquality %>% na.omit
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter(data, y=Ozone, x =Temp, discrete=FALSE)
