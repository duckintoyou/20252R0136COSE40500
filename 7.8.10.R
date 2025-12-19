autoplot(ukcars)

seasadjusted_ukcars <- ukcars %>% stl(s.window = 10, robust = TRUE) %>% seasadj() 
autoplot(seasadjusted_ukcars)

forecast_ukcars1 <- ukcars %>% stlf(h = 4, etsmodel = "AAN", damped = TRUE)
autoplot(forecast_ukcars1)

forecast_ukcars2 <- ukcars %>% stlf(h = 4, etsmodel = "AAN", damped = FALSE)
autoplot(forecast_ukcars2)

ets_ukcars <- ets(ukcars)
autoplot(forecast(ets_ukcars, h = 4))


accuracy(forecast_ukcars1)
accuracy(forecast_ukcars2)
accuracy(ets_ukcars)
# STL + ETS(A, Ad, N) was the best model.

checkresiduals(forecast_ukcars1)
