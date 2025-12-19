# 데이터 불러오기
d <- read.table("stockmarket.dat", header = TRUE)

####### < a >
# 암스테르담 시리즈 그리기
plot(d$Amsterdam, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Amsterdam Stock Market Series")

# 차분 계산 및 그리기
amsterdam_diff <- diff(d$Amsterdam)
plot(amsterdam_diff, type = "l", col = "red", xlab = "Time", ylab = "Differences", main = "First-Order Differences of Amsterdam Series")


fit_arima_010 <- arima(d$Amsterdam, order = c(0, 1, 0))
fit_arima_110 <- arima(d$Amsterdam, order = c(1, 1, 0))
fit_arima_011 <- arima(d$Amsterdam, order = c(0, 1, 1))
fit_arima_111 <- arima(d$Amsterdam, order = c(1, 1, 1))

####### < b >
#AIC는 모델 간의 상대적 비교에 사용. 다른 모델들과 비교했을 때 
#가장 낮은 AIC 값을 가진 모델이 가장 적합한 모델로 간주

fit_arima_010 <- arima(d$Amsterdam, order = c(0, 1, 0))
fit_arima_110 <- arima(d$Amsterdam, order = c(1, 1, 0))
fit_arima_011 <- arima(d$Amsterdam, order = c(0, 1, 1))
fit_arima_111 <- arima(d$Amsterdam, order = c(1, 1, 1))

aic_values <- c(fit_arima_010$aic, fit_arima_110$aic, fit_arima_011$aic, fit_arima_111$aic)

# 가장 낮은 AIC를 가진 모델 선택
best_model_index <- which.min(aic_values)
best_model <- c("ARIMA(0,1,0)", "ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)")[best_model_index]

print(paste("Best fitting model:", best_model))

##### < c > 
fit_arima_best <- fit_arima_010 
# 잔차 코렐로그램 생성 (ACF, PACF)
acf(fit_arima_best$residuals)
pacf(fit_arima_best$residuals)

# 제곱 잔차 코렐로그램 생성
acf(fit_arima_best$residuals^2)
pacf(fit_arima_best$residuals^2)


##### < d > 
install.packages("tseries")
library(tseries)

# 잔차 데이터가 'residuals' 변수에 있다
residuals <- fit_arima_best$residuals

# 각 GARCH 모델 적합
fit_garch10 <- garch(residuals,order=c(1,0))
fit_garch11 <- garch(residuals,order=c(1,1))
fit_garch20 <- garch(residuals,order=c(2,0))
fit_garch21 <- garch(residuals,order=c(2,1))

# 각 모델의 AIC 및 BIC 비교
summary(fit_garch10)
summary(fit_garch11)
summary(fit_garch20)
summary(fit_garch21)

#fit_garch11 의 Box-Ljung Test 가 0.6396 

#
######## < e > 
# 잔차와 잔차 제곱 추출
residuals_best <-na.omit(residuals(fit_garch11))
squared_residuals_best = residuals_best^2

# 잔차의 코렐로그램
acf(residuals_best, main = "ACF of Residuals from Best Fitting GARCH Model")
pacf(residuals_best, main = "PACF of Residuals from Best Fitting GARCH Model")

# 잔차 제곱의 코렐로그램
acf(squared_residuals_best, main = "ACF of Squared Residuals from Best Fitting GARCH Model")
pacf(squared_residuals_best, main = "PACF of Squared Residuals from Best Fitting GARCH Model")

