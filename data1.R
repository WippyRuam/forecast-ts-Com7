library(vars)
library(ggplot2)
library(forecast)

data <- read.csv('C:/Users/Windy/Desktop/Coding/DataSET/COM7BK.csv')

length(as.numeric(data$Open))