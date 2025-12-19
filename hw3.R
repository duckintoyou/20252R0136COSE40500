library(fpp2)
library(astsa)
plot.ts(unemp)
acf2(unemp)

seasonaldiff=diff(unemp, 12)
plot(seasonaldiff)

acf2(seasonaldiff)

unemp.fit <- sarima(unemp, 3,0,2,0,1,1,12)  
