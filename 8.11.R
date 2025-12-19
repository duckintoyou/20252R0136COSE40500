library(fpp2)

#a
ar <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100){
  ar[i] <- 2*ar[i-1] + e[i]
}
  

#b
autoplot(ar) # phi 바꾼거 비교 해야됨 


#c
ma <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100){
  ma[i] <- e[i] - 1*e[i-1]
}
 

#d 
autoplot(ma) # 세타 바꾼거 여러개 해야됨

#e
arma <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100){
  arma[i] <- 0.6*arma[i-1] + e[i]- 0.6*e[i-1]  
}

autoplot(arma)
#f

ar2<- ts(numeric(100))
e <- rnorm(100)
for(i in 3:100){
  ar2[i] <- -0.8*ar2[i-1] + 0.3*ar2[i-2] + e[i]
}
autoplot(ar2)

#g
autoplot

