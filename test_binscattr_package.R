library(datasets)
library(tidyverse)
library(binscattr)

data <- airquality %>% na.omit
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter(data, y=Ozone, x =Temp, discrete=FALSE)

binscatter(data %>% filter(Month==5), y=Ozone, x =Temp, discrete=FALSE)

data$group = factor(data$Month)
binscatter_by_group(data, y=Ozone, x =Temp, discrete=FALSE, group_by=Month)
binscatter_by_group(data, y=Ozone, x =Temp, discrete=FALSE, group_by=group)


