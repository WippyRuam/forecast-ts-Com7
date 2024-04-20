library('forecast')
library('tidyverse')
library('ggplot2')
library('vars')
library('dplyr')
library('stargazer')
library('tseries')

#VAR
data <- read.csv('C:/Users/Windy/Desktop/Coding/DataSET/COM7BK.csv')
data1 <- read.csv('C:/Users/Windy/Desktop/Coding/DataSET/KBANK.BK.csv')
data2 <- read.csv('C:/Users/Windy/Desktop/Coding/DataSET/AOT.BK.csv')

open <- ts(data$Open,start = c(2015,1),end = c(2024,30),frequency = 365)
open1 <- ts(data1$Open,start = c(2015,1),end = c(2024,30),frequency = 365)
open2 <- ts(data2$Open,start = c(2015,1),end = c(2024,30),frequency = 365)

col <- cbind(open,open1,open2)

adf.test(open) 
adf.test(open1) 
adf.test(open2)

VARselect(col, lag.max = 3)

fit <- VAR(col,p = 2, type = "const")

summary(fit)

fcst <- predict(fit, n.ahead = 30, ci = 95 ,dumvar =NULL)

#----------------------------------------------------#
#ARIMA 

acf(open)
pacf(open)

lor <- log(open[2:length(open)] 
           / open[1:length(open)-1])
lorr <- diff(log(open))

fit <- Arima(open, order = c(1,0,1),)
fit_log <- Arima(lor,order = c(1,0,1))

fcst <- forecast(fit, h =30)
fcst_log <- forecast(fit_log,30)

fcst_up <- numeric()
fcst_low <- c()

fcst_up <- numeric()

for (i in 1:30) {
  dec <- fcst$upper[,i] %% 1
  ai <- dec - (fcst$mean[i] %% 1)
  fcst_up <- ai
}

sult <- fcst$mean + fcst_up



