n <- 128
t <- 1:n

# 시리즈 생성
xt1 <- 2 * cos(2 * pi * 0.06 * t) + 3 * sin(2 * pi * 0.06 * t)
xt2 <- 4 * cos(2 * pi * 0.10 * t) + 5 * sin(2 * pi * 0.10 * t)
xt3 <- 6 * cos(2 * pi * 0.40 * t) + 7 * sin(2 * pi * 0.40 * t)

# 종합된 시리즈
xt <- xt1 + xt2 + xt3

# 시리즈 플롯
par(mfrow = c(2, 2))
plot.ts(xt1, ylim = c(-10, 10), main = "xt1 with n=128")
plot.ts(xt2, ylim = c(-10, 10), main = "xt2 with n=128")
plot.ts(xt3, ylim = c(-10, 10), main = "xt3 with n=128")
plot.ts(xt, ylim = c(-16, 16), main = "xt (Sum) with n=128")

# 스케일링 여부는 ... 잘모르겟서 준민 예시는 했는데 문제는 하라는말이없수


# 피리오도그램 계산 (스케일링 없음)
P_unscaled <- abs(fft(xt))^2
Fr <- (0:(n-1))/n

# 피리오도그램 플롯
plot(Fr, P_unscaled, type="o", xlab="frequency", ylab="periodogram", main="Periodogram of xt")

P <- Mod(2 * fft(xt)/n)^2
Fr <- (0:(n-1))/n

# 피리오도그램 플롯
plot(Fr, P, type="o", xlab="frequency", ylab="scaled periodogram", main="Periodogram of xt")


# 시리즈 생성 (n = 100)
n <- 100
t <- 1:n
set.seed(123) # 재현 가능한 결과를 위한 시드 설정
wt <- rnorm(n, mean=0, sd=5) # 표준편차가 5인 정규분포 잡음

# 주어진 시리즈 생성
xt1 <- 2 * cos(2 * pi * 0.06 * t) + 3 * sin(2 * pi * 0.06 * t)
xt2 <- 4 * cos(2 * pi * 0.10 * t) + 5 * sin(2 * pi * 0.10 * t)
xt3 <- 6 * cos(2 * pi * 0.40 * t) + 7 * sin(2 * pi * 0.40 * t)

# 잡음 추가
xt <- xt1 + xt2 + xt3 + wt

# 데이터 시뮬레이션 플롯
plot(t, xt, type="l", main="Simulated Time Series with Noise")

# 피리오도그램 계산 및 플롯
P <- abs(fft(xt))^2  # 스케일링된 피리오도그램을 계산하기 위해 n으로 나눔
Fr <- (0:(n-1))/n
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram", main="Periodogram of xt with Noise")
