library(datasets)
library(tidyverse)
install.packages("binscattr", repos = NULL, type="source")
library(binscattr)


data <- airquality %>% na.omit
binscatter(data, y="Ozone", x ="Temp", discrete=FALSE, controls=c("Solar.R", "Wind" ))
binscatter(data, y=Ozone, x =Temp, discrete=FALSE)

binscatter(data %>% filter(Month==5), y=Ozone, x =Temp, discrete=FALSE)

data$group = factor(data$Month)
binscatter_by_group(data, y=Ozone, x =Temp, discrete=FALSE, grouping_var=Month)

binscatter_by_group(data, y=Ozone, x =Temp, discrete=FALSE, grouping_var=group)

binscatter_basic(data, y=Ozone, x =Temp, discrete=FALSE)
binscatter(data, y=Ozone, x =Temp, discrete=FALSE)
binscatter(data, y=Ozone, x =Temp, discrete=FALSE, grouping_var=Month )
binscatter(data, y=Ozone, x =Temp, controls=c("Solar.R", "Wind" ), discrete=FALSE, grouping_var=Month)
