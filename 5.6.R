library(fpp2)
gas<-window(gasoline,start=1991,end=2005)

# a, b
for (g in 1:26){
  checkgas<-tslm(gas~trend+fourier(gas,K=g))
  print(CV(checkgas)[1:3])
}

ans <- tslm(gas~ trend + fourier(gas, K=1))
plot(gas,fitted.values(ans))
ans <- tslm(gas~ trend + fourier(gas, K=10))
autoplot(ans)
ans <- tslm(gas~ trend + fourier(gas, K=25))
autoplot(ans)


#c
ans <- tslm(gas~ trend + fourier(gas, K=7))
checkresiduals(ans)
#d
fcv <- forecast(ans, newdata=data.frame(fourier(gas,K = 7, 52)))

#e
autoplot(fcv)

