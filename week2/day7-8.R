# 18.07.10

sample(1:40, 5); 

sample(1:40, 5,  replace=T)

sample(c("H", "T"),  10,  replace=T)

sample(c("H", "T"),  10,  replace=T,  prob=c(0.9,  0.1)) 

1/prod(40:36)

prod(40:1)

1/choose(40, 5) 

plot(rnorm(1000, 0, 1)); plot(rnorm(1000, 1, 2)) 

x <- seq(-4, 4, 0.1)
plot(x, dnorm(x))
plot(x, dnorm(x), type="l");  curve(dnorm(x),  from=-4,  to=4)

#이항 분포 위에 정규분포 입힌 그래프 -> 데이터수 10개
x <- 0:10
plot(x, dbinom(x,  size=10,  prob=.33),  type="h")
lines(dnorm(x, 10*.33, sqrt(10*.33*(1-.33))))

#새로운 윈도우 창 띄우기
window()

#이항분포 위에 정규분포 입힌 그래프 -> 데이터수 50개
x <- 0:50
plot(x, dbinom(x,  size=50,  prob=.33),  type="h");
lines(dnorm(x, 50*.33, sqrt(50*.33*(1-.33))))

plot(x, dbinom(x,  size=50,  prob=.50),  type="h");
lines(dnorm(x, 50*.50, sqrt(50*.50*(1-.50))))


# 표준정규분포 100개의 샘플을 얻는다
x <- rnorm(100)
# 히스토그램을 그린다
hist(x, freq=F)
# 그림 위에 정규분포를 입힌다, 키 포인트는 add
curve(dnorm(x), add=T)

# 위에것과 차이는 히스토그램하고 scale이 맞지 않는 문제를 해결
# 계산된 히스토그램을 숨겨놨다가 그린후 맞춘다.
h <- hist(x,  plot=F)
ylim <- range(0,  h$density,  dnorm(0))
hist(x,  freq=F,  ylim=ylim)
curve(dnorm(x),  add=T)

# n개를 sorting해서 그림을 그린다.
n <- length(x)
plot(sort(x), (1:n)/n, type="s", ylim=c(0, 1))
# 
qqnorm(x)

# range
range(x)
# range(x)[1] : min 값, [2] : max값.
xr <- seq(range(x)[1], range(x)[2], length.out = 100)
# pnorm:누적정규분포-> 전체면적이 다 더해서 1이 되는 분포
plot(xr, pnorm(xr), lty="dashed")
plot(ecdf(x), add=TRUE, xlim=range(x))
# 하얀그래프는 이론상 누적, 까만그래프는 경험상 누적

# ks-test:콜모고로프 스미노프 테스트
# data x 가 정규분포와 비슷한가
ks.test(x, "pnorm")
#정규분포 평균 5 분산 1의case
ks.test(x, "pnorm", 5, 1)
#정규분포 평균 5 분산 1의case
ks.test(x, "pnorm", 5, 1, alternative="l")

#-5에서 5까지 0.1씩 자른다
x <- seq(-5,5,0.1)
# 누적정규분포 그림

# 데이터를 정규분포 1에 대해서 그린다 
# 그림이 꼬이기 시작하면 정규분포와 벗어나고 있다고 생각하면됨
plot(x,  pnorm(x),  type="l"); 
lines(x,  pnorm(x, 1, 1),  type="l",  lty=2); 
lines(x,  pnorm(x, 1, 2),  type="l",  lty=3)

# pnorm : 정규분포함수
pnorm(1); 
1-pnorm(1)

# 이항분포값, 50번던져서 0.33확률로 15번 나올 확률
pbinom(15,  50,  .33); 
# 이항분포 평균np와 분산 npq => 정규분포(np, npq)
pnorm(15,  50*.33,  sqrt(50*.33*(1-.33)))

# 이항분포와 정규분포를 비교하는 것(위와같음)
pbinom(150,  500,  .33); 
pnorm(150,  500*.33,  sqrt(500*.33*(1-.33)))

# 분포 그래프 정규분포로는 비교가 힘드니 누적분포 그래프로 비교한다
xr <- seq(0,50)
plot(xr, pnorm(xr,  50*.33,  sqrt(50*.33*(1-.33))),type="l",lty=2,ylab="cumulative")
lines(pbinom(xr,  50,  .33))
# 확인할때 ks.test를 사용한다.
ks.test(pbinom(15,  50,  .33),pnorm(15,  50*.33,  sqrt(50*.33*(1-.33))))


## 평균, 분산, 표준변차, 중앙값, 사분위수 
x <- rnorm(50)
mean(x)

sd(x)

var(x)

median(x)

quantile(x); quantile(x,  0.3)

summary(x); attributes(summary(x)); 

x
x <- as.matrix(x); 
apply(x,  1,  sum); 
apply(x,  2,  sum); 
apply(x,  2,  mean); 
apply (x,  1,  function(x) x+2*x^2); x+2*x^2

#
xbar <- 83
sigma <- 12
n <- 5
sem <- sigma/sqrt(n)
sem
xbar + sem * qnorm(0.025)
xbar + sem * qnorm(0.975)

# norm.interval 함수
norm.interval = function(data, variance = var(data), conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sigma = sqrt(variance/length(data))
  c(xbar - z * sigma, xbar + z * sigma)}

install.packages("psych"); library(psych); describe(x)

install.packages("ISwR"); library(ISwR); attach(juul); data(juul) 
juul; summary(juul)
mean(age); mean(age,  na.rm=T) # 결측치를 처리하여 평균구하기



juul$sex <- factor(juul$sex,  labels=c("M", "F"))
juul$menarche <- factor(juul$menarche,  labels=c("No", "Yes"))
juul$tanner <- factor(juul$tanner,  labels=c("I", "II", "III", "IV", "V"))
juul


juul <- transform(juul, 
                  sex=factor(sex, labels=c("M", "F")), 
                  menarche=factor(menarche, labels=c("No", "Yes")), 
                  tanner=factor(tanner, labels=c("I", "II", "III", "IV", "V")))

# age에 대한 데이터로 실험
x <- juul$age
hist(x)
hist(x, freq = T)
hist(x)
hist(x, freq = F)
mid.age <- c(2.5, 7.5, 13, 16.5, 17.5, 19, 22.5, 44.5, 70.5)
acc.count <- c(28, 46, 58, 20, 31, 64, 149, 316, 103)
age.acc <- rep(mid.age, acc.count)
brk <- c(0, 5, 10, 16, 17, 18, 20, 25, 60, 80)
# 안지정해주면 기본값이나 breaks 지정값을 주면 이에 맞춰서 그리게됨
hist(age.acc, breaks=brk)


## boxplot 그래프 
par(mfrow=c(1, 2))
boxplot(x)
boxplot(log(x))


# 에너지를 불러옴
attach(energy)
energy
expend.lean <- expend[stature=="lean"]
expend.obese <- expend[stature=="obese"]
par(mfrow=c(2, 1))
hist(expend.lean, breaks=10, xlim=c(5, 13), ylim=c(0, 4), col="white")
hist(expend.obese, breaks=10, xlim=c(5, 13), ylim=c(0, 4), col="grey")
par(mfrow=c(1, 1))
boxplot(expend ~ stature)


par(mfrow=c(2, 2),  mex=0.8,  mar=c(3, 3, 2, 1)+.1)
stripchart(expend ~ stature)
stripchart(expend ~ stature,  method="stack")
stripchart(expend ~ stature,  method="jitter")
stripchart(expend ~ stature,  method="jitter",  jitter=.03)
stripchart(list(lean=expend.lean,  obese=expend.obese)) 

## 7.10



## 7.11
x <- matrix(c(652, 1537, 598, 242, 36, 46, 38, 21, 218, 327, 106, 67),  nrow=3, byrow=T)
x
colnames(x) <- c("0", "1-150", "151-300", "300")
rownames(x) <- c("Married", "Prev.married", "Single")
x
names(dimnames(x)) <- c("marital", "consumption")
x
x.data.frame <-  as.data.frame(as.table(x))
x.data.frame
attach(x.data.frame)
table(marital)
table(marital,  consumption)
xtabs(~ marital + consumption)
x.table <- table(marital, consumption)
margin.table(x.table, 1)
margin.table(x.table, 2)
prop.table(x.table, 1)
prop.table(x.table, 2)
total.x <- margin.table(x, 1)
total.x

barplot(total.x,  col="white")
par(mfrow=c(2, 2))
barplot(x,  col="white")
barplot(t(x),  col="white")
barplot(t(x),  col="white",  beside=T)
barplot(prop.table(t(x), 2),  col="white",  beside=T)
par(mfrow=c(1, 1))
barplot(prop.table(t(x), 2), beside=T,  legend.text=colnames(x),  col=c("white", "grey80", "grey50", "black"))
barplot(prop.table(t(x), 2), beside=F,legend.text=colnames(x),  col=c("white", "grey80", "grey50", "black"))
barplot(prop.table(t(x), 2), beside=T)
legend("top",  legend=colnames(x),  col=c("white", "grey80", "grey50", "black"), lty=1:4,  cex=0.8)
legend(locator(1),  legend=colnames(x),  col=c("white", "grey80", "grey50", "black"), lty=1:4,  cex=0.8)


dotchart(t(x),  lcolor="black")


opar <- par(mfrow=c(2, 2), mex=0.8,  mar=c(1, 1, 2, 1))
slices <- c("white", "grey80", "grey50", "black")
pie(x["Married", ],  main="Married",  col=slices)
pie(x["Prev.married", ],  main="Previously married",  col=slices)
pie(x["Single", ],  main="Single",  col=slices)
par(opar)#그래프 포맷 유지하고 싶다면



tapply(age,  sex,  mean)
tapply(age,  sex,  sd)
tapply(age,  sex,  length)
xbar <- tapply(age,  sex,  mean)
s <- tapply(age,  sex,  sd)
n <- tapply(age,  sex,  length)
cbind(mean=xbar,  std.dev=s,  n=n)
aggregate(juul[c("age", "igf1")],  list(sex=juul$sex),  mean,  na.rm=T)
aggregate(juul[c("age", "igf1")],  juul["sex"],  mean,  na.rm=T)
by(juul,  juul["sex"],  summary)


#One sample t-test
daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)
t.test(daily.intake, mu=7725)
wilcox.test(daily.intake,  mu=7725) # 비모수 검정



#Two sample t-test
attach(energy)
energy
t.test(expend~stature)
t.test(expend~stature,  var.equal=T) # 분산 동일성이 보장될 경우
var.test(expend~stature) #분산 동일성 검정
wilcox.test(expend~stature) # 비모수 검정



#Paired t-test
attach(intake)
intake
post - pre
t.test(pre,  post,  paired=T) # 분산 동일성 불필요,  관측치 동일
t.test(pre,  post) # 일반적인 t-test,  결과 비교
wilcox.test(pre,  post,  paired=T) # 비모수 분석


prop.test(83, 100, 0.75)

attach(thuesen)
cor(blood.glucose, short.velocity) 
cc <- complete.cases(thuesen) # missing 데이터 찾기
cor(blood.glucose, short.velocity, use="complete.obs")
cor.test(blood.glucose, short.velocity, use="complete.obs")
cor.test(blood.glucose, short.velocity, method="spearman", use="complete.obs")


lm(short.velocity~blood.glucose)
summary(lm(short.velocity~blood.glucose))
plot(blood.glucose, short.velocity)
abline(lm(short.velocity~blood.glucose))
lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
segments(blood.glucose, fitted(lm.velo),  blood.glucose, short.velocity)
plot(fitted(lm.velo), resid(lm.velo)) # 오류 발생 
options(na.action=na.exclude) #missing 데이터 제거
lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
plot(blood.glucose, short.velocity)
segments(blood.glucose, fitted(lm.velo),  blood.glucose, short.velocity)
plot(fitted(lm.velo), resid(lm.velo)) #잔차분석
qqnorm(resid(lm.velo))


pred.frame <- data.frame(blood.glucose=4:20)
pp <- predict(lm.velo,  int="p",  newdata=pred.frame) # 구간 전체 기준
pc <- predict(lm.velo,  int="c",  newdata=pred.frame) # 주어진 데이터 기준
plot(blood.glucose, short.velocity,  ylim=range(short.velocity,  pp,  na.rm=T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc,  pc,  lty=c(1, 2, 2),  col="black")
matlines(pred.gluc,  pp,  lty=c(1, 3, 3),  col="black")



attach(red.cell.folate)
summary(red.cell.folate)
anova(lm(folate~ventilation))
summary(lm(folate~ventilation))
pairwise.t.test(folate,  ventilation,  p.adj="bonferroni")
oneway.test(folate~ventilation) # 분산 동일성 체크
pairwise.t.test(folate, ventilation, pool.sd=F) #동일하지 않다면
xbar <- tapply(folate,  ventilation,  mean)
s <- tapply(folate,  ventilation,  sd)
n <- tapply(folate,  ventilation,  length)
sem <- s/sqrt(n)
stripchart(folate~ventilation,  method="jitter", jitter=0.05,  pch=16,  vert=T)
arrows(1:3, xbar+sem, 1:3, xbar-sem, angle=90, code=3, length=.1)
lines(1:3, xbar, pch=4, type="b", cex=2)

attach(heart.rate) 
heart.rate
anova(lm(hr~subj+time))
interaction.plot(time,  subj,  hr)
interaction.plot(ordered(time), subj, hr)
