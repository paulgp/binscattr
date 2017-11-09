library(datasets)
library(tidyverse)
library(binscattr)

data <- airquality %>% na.omit
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter(data, y=Ozone, x =Temp, discrete=FALSE)

data$group = factor(data$Month)
binscatter_by_group(data, y=Ozone, x = Temp, discrete=FALSE, grouping_var=Month)
binscatter_by_group(data, y=Ozone, x = Temp, discrete=FALSE, grouping_var=group, fitline = F, connectdots = TRUE, bins = 10)

binscatter_basic(data, y = Ozone, x = Temp, discrete = FALSE, connectdots=TRUE)
binscatter_basic(data, y = Ozone, x =Temp, discrete = FALSE, connectdots=FALSE)
binscatter_basic(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter_basic(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ), connectdots=TRUE)


binscatter(data, y=Ozone, x =Temp)
binscatter(data, y=Ozone, x =Temp, fitline=F)
binscatter(data, y=Ozone, x =Temp, fitline=T, connectdots=T)
binscatter(data, y=Ozone, x =Temp, grouping_var=Month )
binscatter(data, y=Ozone, x =Temp, grouping_var=Month, fitline=F )
binscatter(data, y=Ozone, x =Temp, grouping_var=Month, fitline=F , connectdots=T)
binscatter(data, y=Ozone, x =Temp, grouping_var=Month, fitline=T , connectdots=T, bins = 10)

binscatter(data, y=Ozone, x =Temp, controls=c("Solar.R", "Wind" ))
binscatter(data, y=Ozone, x =Temp, controls=c("Solar.R", "Wind" ), grouping_var=group)
binscatter(data, y=Ozone, x =Temp, controls=c("Solar.R", "Wind" ), grouping_var=group, fitline=F , connectdots=T)
