library(fpp2)
arr <- ts(numeric(100))
e <- rnorm(100)
for(i in 3:100){
  arr[i] <- -0.9*arr[i-2] + e[i]
}

gglagplot(arr)
