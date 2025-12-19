library(fpp2)

#a. Plot the data using autoplot. Why is it useful to set facets=TRUE?
str(advert)
head(advert)

autoplot(advert, facets = TRUE)
# Can see the advertising expenditure data and sales volume data in different panels. facets = TRUE option can plot the subsets of data in each panel.

# b. Fit a standard regression model yt = a + b*xt + nt where yt denotes sales and xt denotes advertising using the tslm() function.
advert_tslm <- tslm(sales ~ advert, data = advert)

# c. Show that the residuals have significant autocorrelation.
checkresiduals(advert_tslm)
# The residuals have significant autocorrelations at lag 1 and 2.

# d. What difference does it make you use the function instead:
#  Arima(advert[,"sales"], xreg=advert[,"advert"], order=c(0,0,0))
advert_dreg000 <- Arima(
  advert[, "sales"], xreg = advert[, "advert"],
  order = c(0, 0, 0)
)

checkresiduals(advert_dreg000)
advert_tslm$residuals - advert_dreg000$residuals
# The residuals from dynamic regression model are almost same as the residuals from tslm function.
# But when I use Arima function, I can do ARIMA modeling for residuals by designating order.

# e. Refit the model using auto.arima(). How much difference does the error model make to the estimated parameters? What ARIMA model for the errors is selected?
advert_dreg.auto <- auto.arima(
  advert[, "sales"], xreg = advert[, "advert"]
)

advert_dreg000
# error model coefficients:
# intercept : 78.7343, slope_advert : 0.5343

advert_dreg.auto
# error model : ARIMA(0, 1, 0)
# xreg : 0.5063

# f. Check the residuals of the fitted model.
checkresiduals(advert_dreg.auto)
# The residuals are like white noise.

autoplot(advert[, "sales"], series = "Data") +
  geom_line(color = "red", size = 1) +
  autolayer(advert_dreg.auto$fitted, size = 1, series = "Dynamic Regression fitted values") +
  autolayer(advert_tslm$fitted.values, size = 1, series = "Linear Regression fitted values") +
  ylab("Sales volume")

accuracy(advert_dreg000)
accuracy(advert_dreg.auto)
# The plot above and most of errors show that dynamic regression with ARIMA(0, 1, 0) error model was better than the linear regression model.

# g. Assuming the advertising budget for the next six months is exactly 10 units per month, produce and plot sales forecasts with prediction intervals for the next six months.
fc_advert_dreg.auto <- forecast(
  advert_dreg.auto, h = 6,
  xreg = rep(10, 6)
)

autoplot(fc_advert_dreg.auto)
# The forecasts are like the result of naive method.