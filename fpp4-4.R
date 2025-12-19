### 9
library(fpp2)
data("usgdp")

# (a) Box-Cox 변환을 찾기
# a. if necessary, find a suitable Box-Cox transformation for the data;

lambda <- BoxCox.lambda(usgdp)
usgdp_transformed <- BoxCox(usgdp, lambda)
plot(usgdp_transformed)

# (b) auto.arima()을 사용하여 변환된 데이터에 대한 ARIMA 모델 적합
fit_arima <- auto.arima(usgdp, lambda = lambda)
autoplot(usgdp, series = "Data") + autolayer(fit_arima$fitted, series = "Fitted")
usgdp_autoarima
#ARIMA(2,1,0) with drift 

# (c) 다른 순서를 가진 플로저블 모델들을 시도해보기

fit_arima210 <- arima(usgdp_transformed, order = c(2,1,0))

# c. try some other plausible models by experimenting with the orders chosen;
ndiffs(BoxCox(usgdp, lambda_usgdp))
# the data need 1 first differencing to be stationary.
ggtsdisplay(diff(BoxCox(usgdp, lambda_usgdp)))
# ACF plot shows sinusoidal decrease while PACF plot shows significant spikes at lag 1 and 12. I think that I can ignore the spike at lag 12 because the data are aggregated quarterly, not monthly. Therefore, I'll experiment with ARIMA(1, 1, 0) model.
usgdp_arima.1.1.0 <- Arima(
  usgdp, lambda = lambda_usgdp, order = c(1, 1, 0)
)

usgdp_arima.1.1.0

autoplot(usgdp, series = "Data") +
  autolayer(usgdp_arima.1.1.0$fitted, series = "Fitted")

# I'll also try ARIMA(1, 1, 0) with drift model.
usgdp_arima.1.1.0.drift <- Arima(
  usgdp, lambda = lambda_usgdp, order = c(1, 1, 0),
  include.drift = TRUE
)

usgdp_arima.1.1.0.drift

autoplot(usgdp, series = "Data") +
  autolayer(usgdp_arima.1.1.0.drift$fitted, series = "Fitted")
# It looked like that these models also fit well to the data.

# d. choose what you think is the best model and check the residual diagnostics;
accuracy(usgdp_autoarima)
accuracy(usgdp_arima.1.1.0)
accuracy(usgdp_arima.1.1.0.drift)
# Some errors show that ARIMA(2, 1, 0) with drift is the best model while others show that ARIMA(1, 1, 0) with drift is the best. Check the residuals of both cases.
checkresiduals(usgdp_autoarima)
checkresiduals(usgdp_arima.1.1.0.drift)
# In either case, the residuals are like white noise series and are not normally distributed.
# I'll choose the best model as ARIMA(2, 1, 0) with drift model. With the model, RMSE and MASE values were lower. And there wasn't significant spike at lag 2 in ACF plot of ARIMA(2, 1, 0) with drift model, even if it exists in ARIMA(1, 1, 0) with drift model.

# e. produce forecasts of your fitted model. Do the forecasts look reasonable?
fc_usgdp_autoarima <- forecast(
  usgdp_autoarima
)

autoplot(fc_usgdp_autoarima)
# It looked like the forecasts are reasonable.

# f. compare the results with what you would obtain using ets() (with no transformation).
fc_usgdp_ets <- forecast(
  ets(usgdp)
)

autoplot(fc_usgdp_ets)
# It looked like these forecasts are more likely than the ones with ARIMA model. When trend is obvious, is ETS better than ARIMA model? I wonder about it.

```


### 10
data("austourists")

# 필요한 패키지를 불러옵니다.
library(forecast)

# (a) 시간 플롯을 그립니다.
austourists_data <- austourists
plot(austourists_data, main='Time plot of quarterly visitor nights', xlab='Year', ylab='Millions of nights')

# (b) ACF 그래프를 그립니다.
Acf(austourists_data, main='ACF of austourists')

# (c) PACF 그래프를 그립니다.
Pacf(austourists_data, main='PACF of austourists')

# (d) 계절 차분 데이터의 플롯을 생성합니다.
seasonally_diff <- diff(austourists_data, lag=4)
plot(seasonally_diff, main='Seasonally differenced austourists data', xlab='Year', ylab='Differenced millions of nights')
plot(diff(seasonally_diff, lag = 4))
# 해당 그래프에서 제안되는 모델을 추측합니다.
# 이 부분은 그래프를 해석하여 적절한 모델을 결정하는 분석가의 몫입니다.

# (e) auto.arima()를 사용하여 모델을 적합시키고 결과를 확인합니다.
fit <- forecast(
  auto.arima(austourists)
)

fit$model
# auto.arima gave ARIMA(1, 0, 0)(1, 1, 0)[4] model.

fc_austourists_arima.1.1.0.1.1.0.4 <- forecast(
  Arima(austourists, 
        order = c(1, 1, 0), 
        seasonal = c(1, 1, 0))
)


fc_austourists_arima.1.1.0.1.1.0.4$model
accuracy(fit)
accuracy(fc_austourists_arima.1.0.0.1.1.0.4)

checkresiduals(fit)
# auto.arima()가 제안한 모델이 초기에 추측한 모델과 일치하는지 확인합니다.
# 이는 주어진 데이터와 R의 출력을 기반으로 판단해야 합니다.

# (f) 모델을 백시프트 연산자로 쓴 뒤, 백시프트 연산자를 사용하지 않고 씁니다.
# fit 객체의 결과를 사용하여 모델을 적습니다.
cat('Model with backshift operator:\n')
print(fit$model)

# 모델을 백시프트 연산자를 사용하지 않고 쓰기
# 이 부분은 모델의 파라미터를 해석하여 수식으로 직접 작성해야 합니다.

