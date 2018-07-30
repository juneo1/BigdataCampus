#============================================================================
# 7월 18일 실습문제
# 5교시: (실습을 통해서 개념 완성)

Y<- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
X1<- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2<- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
X3<- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4<- c(60,52,20,47,33,22,6,44,22,26,34,12,12)

cement<- data.frame(Y,X1,X2,X3,X4) 
cement

#q1) 아래 프로그램을 이용하여 다음 회귀모형에 데이터를 적합시켜라.
model<- lm(Y ~., data=cement)  # Y ~. 은 Y2 ~ X1 + X2 + X3 + X4와 같음
summary(model)         # 결과요약

#q2) 사전 정보에 의하면 독립변수들이 Y에 영향을 준다고 하였는데, 회귀분석 결과 유의하지 않다고 나왔다. 이는 독립변수들 간에 강한 상관관계가 나타나는 다중공선성 현상이 있을 수 있기 때문인데, 가장 간단하게 다중공선성을 알아보는 방법은 분산팽창계수(VIF)의 값을 구해서 10보다 크면 다중공선성이 존재한다고 한다. 독립변수 X1부터 X4 사이에 다중공선성 현상이 존재한다고 할 수 있는가? 
install.packages("car")  # vif 함수가 들어있는 car 패키지 설치
library(car)  # car 패키지 적재
vif(model)  # 위에서 만든 다중회귀 모델의 다중공선성 확인

#q3) cement 데이터 내의 변수들의 산점도를 이용하여 독립변수들 간에 강한 상관관계가 존재하는 지를 확인하여라.
plot(cement) 

#q4) 전진선택법을 이용한 최적회귀모형을 구하여라.
model.con <- lm(Y~1,data=cement) # 예측인자가 없고 반응인자가 있는 모델
model.forward <- step(model.con, scope=list(lower=model.con,upper=model), direction="forward")  # 전진선택법


#q5) 후진선택법을 이용한 최적회귀모형을 구하여라.
step(model, scope=list(lower=model.con,upper=model), direction="backward")  # 후진선택법

#q6) 단계선택법을 이용한 최적회귀모형을 구하여라.
step(model.con, scope=list(lower=model.con,upper=model), direction="both")  # 단계별선택법

k <- ols_step_best_subset(model)
k
k1 <- ols_step_all_possible(model)
k1

#q7) q6의 결과를 해석한 다음 설명 중 틀린 부분을 지적하라.

#============================================================================

# 6교시: (실습을 통해서 개념 완성)
#q1) 분산분석을 수행하고 결과를 설명하라
result <- lm(Y~X1+X2+X4, data=cement) 
anova(result)

#q2) 회귀계수만 간단하게 프린트하라.
result$coef

#q3) 잔차제곱합 (SSE)를 구하라.
deviance(result)

#q4) 다음 프로그램을 이용하여 회귀계수들의 95% 신뢰구간을 구하여라.
confint(result)

#q5) 다음 프로그램을 이용하여 잔차제곱합 (SSE)를 구하여라.
deviance(result)

#q6) 다음 문장을 이용하여 유의수준 5%에서 아래 가설들을 검정하여라.
summary(result)

#q7) 결정계수 (R-squared)와 수정결정계수 (Adjusted R-squared)의 값을 각각 구하여라.
summary(result)

#q8) 유의수준 1%에서 분산분석의 결과를 다음과 같이 알기 쉽게 설명하여라.
#Ans) 유의확률(p-value)는 (3.323e-08)이고 유의수준 0.01보다 (작아서) 독립변수들의 효과는 (있다)

#q9) 다음 문장을 이용하여 R (lm)에서 제공하는 모형진단 그래프를 작성하라.
par(mfrow=c(2,2))
plot(result)

#q10) q9)의 결과를 이용하여 오차항의 등분산성이 타당한 지 설명하여라.
#q11) q9)의 결과를 이용하여 오차항의 정규성이 타당한 지 설명하여라.

#============================================================================

#7교시: (자력으로 TEST)
#R에 내장되어 있는 quakes 데이터 셋을 이용하여 다음 질문에 답하여라.

head(quakes)
quake.con <- lm(stations ~ 1, data=quakes)
quake.model <- lm(stations ~., data=quakes)

#q1) 전진선택법을 이용한 최적회귀모형을 구하여라.
step(quake.con, scope=list(lower=quake.con,upper=quake.model), scopedirection="forward")

#q2) 후진선택법을 이용한 최적회귀모형을 구하여라.
step(quake.model, scope=list(lower=quake.con,upper=quake.model), direction="backward")

#*q3) 단계선택법을 이용한 최적회귀모형을 구하여라.
step(quake.con, scope=list(lower=quake.con,upper=quake.model), direction="both")

#*q4) 모든 가능한 회귀(all possible regression)를 이용한 최종모형에서 무엇이 가장 좋은 모형인지를 설명하라.
# ex) 변수가 1개짜리~ 늘어나는 표를 만든 다음 분석하는 것

install.packages("olsrr")
library("olsrr")

install.packages("car")
library("car")

car::vif(quake.model) 
(car::vif(quake.model)) > 10

k <- ols_step_best_subset(quake.model)
k
k1 <- ols_step_all_possible(quake.model)
k1


summary(quake.model)
