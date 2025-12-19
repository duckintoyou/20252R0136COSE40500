install.packages("itsmr")
install.packages("tseries")
install.packages("lmtest")

library(itsmr)
library(tseries)
library(lmtest)

# Lake Huron 데이터 불러오기
data("LakeHuron")

# ADF 테스트 수행
adf_test_ar1 <- adf.test(LakeHuron, alternative = "stationary", k = 0)  # AR(1) 모델 가정
adf_test_ar2 <- adf.test(LakeHuron, alternative = "stationary", k = 1)  # AR(2) 모델 가정

# DW 테스트 수행
# AR(1) 및 AR(2) 모델을 사용한 회귀 모델 적합
lm_ar1 <- lm(LakeHuron ~ lag(LakeHuron, -1))
lm_ar2 <- lm(LakeHuron ~ lag(LakeHuron, -1) + lag(LakeHuron, -2))

# DW 테스트 수행
dw_test_ar1 <- dwtest(lm_ar1)
dw_test_ar2 <- dwtest(lm_ar2)

# 결과 출력
print(adf_test_ar1)
print(adf_test_ar2)
print(dw_test_ar1)
print(dw_test_ar2)

