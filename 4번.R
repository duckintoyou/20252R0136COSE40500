install.packages("lubridate")
library("lubridate")
library(fpp2)
osvisit <-read.table("osvisit.dat")
tsosvisit2 <- ts(osvisit,start=1977,frequency = 12)
tsosvisit <- log(tsosvisit2)

#a 
autoplot(tsosvisit)
yt = log(tsosvisit)
# the time series plot cleary show an increasing
# and seasonal trend.
acf(tsosvisit)
acf(yt)
pacf(tsosvisit)

# -> not stationary

#b
os_arima <- arima(tsosvisit, order = c(1,1,0))
os_arima
plot.ts(residuals(os_arima))
sd(residuals(os_arima))
sd(residuals(os_arima))^2
#c
os_arima2 <- arima(tsosvisit, order = c(1,1,0), seasonal = c(0,1,0))
acf(residuals(os_arima2))
pacf(residuals(os_arima2))

#d

d_1  <- arima(tsosvisit, order = c(1,1,0), seasonal = c(1,1,0))
d_2 <- arima(tsosvisit, order = c(0,0,1), seasonal = c(0,1,1))
d_3 <- arima(tsosvisit, order = c(1,1,0), seasonal = c(0,1,1))
d_4 <- arima(tsosvisit, order = c(0,1,1), seasonal = c(1,1,0))
d_5 <- arima(tsosvisit, order = c(1,1,1), seasonal = c(1,1,1))
d_6 <- arima(tsosvisit, order = c(1,1,1), seasonal = c(1,1,0))
d_7 <- arima(tsosvisit, order = c(1,1,1), seasonal = c(0,1,1))

d_1$aic
d_2$aic
d_3$aic
d_4$aic
d_5$aic
d_6$aic
d_7$aic

#e

