#18.07.05 

#4. 10!을 출력하는 프로그램을 작성하시오

factorial <- function(x){
  rst <- 1
  for(i in x:1){
    rst = rst * i
  }
  print(rst)
}

factorial(2)


#7.한 사람이 주민등록번호를 입력하여 생년월일과 남,여 여부를 출력하는 프로그램을 작성하시오.

dat = readline("insert your joomin!")
dat = strsplit(dat, "-")[[1]]
dat = strsplit(dat,"-")
dat[1]
dat[2]
year = substr(dat[1], 1,2)
month = substr(dat[1], 3,4)
day = substr(dat[1], 5,6)
cat(year, "년", month, "월", day, "일생입니다" )

gender = substr(dat[2], 1,1)
if (gender == "1") {
  cat("남자입니다")
} else {
  cat("여자입니다")
}

#7-1 함수화
jumin  = function() {
  dat = readline("주민번호 앞자리 입력")
  jumin_num = as.integer(dat)
  year = round(jumin_num / 10000)
  rem = jumin_num %% 10000
  month = round(rem/100)
  day = rem %% 100
}

#9.for문을 이용하여 합을 구하는 코딩.

hap <-  function(x,y){
  sum <- 0
  for(i in x:y) {
    for(j in 1:i) {
      sum = sum+j
      if(j!=y) {
        cat(j, "+")
      } else {
        cat(y, "=", sum)
      }
    }
  }
}

hap(1,10)


#10. 벡터에 10개의 데이터가 입력되어 있다. 데이터에서 중복되는 데이터들의 수를 출력한다.

data <- c(1,2,1,1,3,2,6,3,4,5)
num <- unique(data)


#11. 


lotto <- function() {
  count <- 0
  
  ans <- sample(1:45, 6)
  print(ans)
  dat <- readline("Insert 6 numbers \n")
  dat = strsplit(dat, " ")[[1]]
  
  
  for(i in 1:6){
    for(j in 1:6){
      if(dat[i] == ans[j]){
        count <- count+1
      }
    }
  }
  
  if(count < 4){
    cat("꽝")
  }
  
  if(count == 6) {
    cat("6개 정답, 1등입니다.")
  } 
  
  if(count == 5) {
    cat("5개 정답, 2등입니다.")
  } 
  
  if(count == 4) {
    cat("4개 정답, 3등입니다.")
  } 
}

lotto()


# 일변량 자료분석

# 한화면에 그래프 여러개 그리기
par(mfrow=c(1,3))
barplot(table(mtcars$carb), main ="Barplot of Carburetors", 
        xlab ="#of carburetors", ylab="frequency", col="blue")
barplot(table(mtcars$cyl), main ="Barplot of Cylender", 
        xlab ="#of cylender", ylab ="frequency", col="red")
barplot(table(mtcars$gear), main ="Barplot of Gear", 
        xlab ="#of gear", ylab ="frequency", col="green")

# exercise
height <- c(9,15,20,6)
name <- c("판매1팀", "판매2팀", "판매3팀", "판매4팀")
barplot(height, names.arg=name, xlab="판매팀", ylab="판매실적(억)", main="부서별 판매 실적", col = rainbow(length(height)))
        

# label옆에 데이터 출력하기
x <- c(10,15,22,16)
y <- c("판매1팀", "판매2팀", "판매3팀", "판매4팀")
pct <- round(x/sum(x)*100)
y <- paste(y, pct) 
y <- paste(y, "%", sep="")
pie(x, labels = y, col=rainbow(length(x)), main="부서별 영업실적")


#3D 원그래프
install.packages("plotrix")
library(plotrix)
x <- c(9,15,20,6)
y <- c("판매1팀", "판매2팀", "판매3팀", "판매4팀")
pie3D(x,labels = y, explode = 0.1, labelcex = 0.8, main="부서별영업실적")


#문제1. 

library(plotrix)
ds <- table(iris[, "Species"])
ds
pie3D(ds, explode = 0.1, labelcex = 0.8, main="종별 도수분포표")


#문제2. 
pg <- table(quakes[, "mag"])
pg
barplot(pg, xlab = "지진강도", ylab = "발생회수", main="지진 강도별 발생 횟수", col = rainbow(length(pg)))


#mean, median, qualtile, summary
mydata <- c(50, 60, 100, 75, 200)
mydata.big <- c(mydata, 50000)
mean(mydata)
mean(mydata.big)
median(mydata)
median(mydata.big)
mean(mydata, trim=0.2)
mean(mydata.big, trim=0.2)
quantile(mydata)
quantile(mydata, (0:10)/10 )
summary(mydata)
fivenum(mydata)


#diff, var, sd
diff(range(mydata))
var(mydata)
sd(mydata)


#boxplot
head(state.x77)
st.income <- state.x77[, "Income"]
st.income
boxplot(st.income, ylab="Income value")


#boxplot2
head(iris)
boxplot(Petal.Width~Species, data = iris, ylab ="Petal.Width")



#histogram
st.income <- state.x77[, "Income"]
hist(st.income, 
     main="Histogram for Income", 
     xlab = "Income",
     ylab = "frequency", 
     border = "blue", 
     col = "green", 
     las = 2, 
     breaks = 5)


#줄기-잎 그림
score <- c(40,55,90,75,59,60,63,65,69,71 )
stem(score, scale = 2)



## 연습문제1

#1. 성적을 score벡터에 저장하시오
score <- c(90,85,73,80,85,65,78,50,68,96)
names(score) <- c("KOR", "ENG", "MATH", "HIST", "SOC", "MUSIC", "BIO", "EARTH", "PHY", "ART")

#2.score벡터의 내용을 보이시오
score

#3.전체성적의 평균
mean(score)

#4.전체 성적의 중앙값
median(score)

#5.전체 성적의 표준편차
sd(score)

#6.가장 성적이 높은 과목의 이름을 보이시오
names(score[score == max(score)])

#7.성적에 대한 boxplot을 그리시오
boxplot(score, ylab="Score")

#8. 성적에 대한 히스토그램을 그리되 title:Hong's score, 막대색: 보라색)
hist(score, main="Hong's Score", col = "purple")


## 연습문제2: mtcars 데이터셋을 이용하여 다음 문제를 해결하시오

#1.중량(wt)의 평균값, 중앙값, 절사 평균값(범위15%), 표준편차를 구하시오
mt.wt <- mtcars[, "wt"]
mean(mt.wt)
median(mt.wt)
mean(mt.wt, trim=0.15)
sd(mt.wt)

#2.중량에 대한 summary함수의 적용결과
summary(mt.wt)

#3.실린더수(cyl)에 대해 도수분포표를 구하시오
mt.cyl <- mtcars[, "cyl"]
table(mt.cyl)

table(mtcars$cyl)

#4.앞에서 구한 도수분포표를 막대그래프로 나타내시오
barplot(table(mt.cyl))

#5. 중량에 대한 히스토그램, cyl, gear에 대한 막대그래프를 한 화면에 보이게 작성하시오
par(mfrow=c(1,3))
hist(mt.wt)
barplot(table(mtcars$cyl))
barplot(table(mtcars$gear))
        
#6.중량에 대한 boxplot
boxplot(mt.wt)

#7.배기량(disp)에 대해 boxplot
boxplot(mtcars$disp)




## 연습문제 3

#1. 데이터를 읽어서 저장한다.
url <- "https://raw.githubusercontent.com/cran/BTYD/master/data/cdnowElog.csv"
cds <- read.csv(url)
head(cds)

#2. cds 거래량에 대한 빈도수를 표로 만들어 출력해 보고 막대그래프로 그려보자.
barplot(table(cds$cds))

#3. Masterid별 거래 건수를 도수분포표로 만들고 원그래프로 그려본다. 그래프 위에는 label,데이터비율도 출력한다.
data <- table(cds$masterid)
pie(data)
names(data)

pct <- round(data/sum(data)*100)
y <- paste(names(data), pct) 
y <- paste(y, "%", sep="")
pie(data, labels = y, col=rainbow(length(x)), main="Masterid별 거래 건수 ")


## 연습문제4 - 추가(책에없음)
#최근 2006~2016년 출생아 수를 막대그래프로 그려보자.

setwd("C:/Users/vic/Documents/R_day")
bd <- read.csv("출생아수.csv", stringsAsFactors = F)
data <- as.matrix(bd[1,-1])
View(data)
colnames(data) <- c("2006", "2007", "2008", "2009","2010","2011","2012", "2013", "2014", "2015","2016")
for(i in 1:11){
  data[1,i] <- as.numeric(data[1,i])%/%10000
}
barplot(data, ylab="출산아수(만명)", xlab="출생년도")



# 연습5 : 2009년~ 2015년 서울특별시 남녀 출산아수를 boxplot을 이용하여 그려본다.
sbd <- read.csv("시군구.csv", header=T ,stringsAsFactors = T)
View(sbd)
data <- as.matrix(sbd[,-c(1,2)])
data <- as.matrix(data[-1,])
View(data)
data <- data[-c(1,4,7,10,13,16,19),]
data <- as.numeric(data)
data

for(i in 1:14){
  data[i] <- data[i]%/%1000
}
names(data) <- c("2009","2009","2010","2010","2011","2011","2012","2012","2013","2013",
                     "2014","2014","2015","2015")
View(data)
boxplot(data, ylab="출생아수(만명)", xlab="1:남, 2:여", main="남녀 출산아수")


##
sbd <- read.csv("시군구.csv", stringsAsFactors = T)

data <- as.matrix(sbd[,-1])
data <- as.matrix(data[-1,])
data <- data[-c(2,5,8,11,14,17),]
View(data)
data_1 <- data[c(3,6,9,12,15,18,21)]
data_2 <- data[c(4,7,10,13,16,19,22)]

data_1 <- as.numeric(data_1)
data_2 <- as.numeric(data_2)

par(mfrow=c(1,2))
boxplot(data_1)
boxplot(data_2)




# 연습6 legend
data2=NULL
sbd <- read.csv("시군구.csv", stringsAsFactors = F)
sbd
data <- as.matrix(sbd[,-1])
data <- as.matrix(data[-1,])


data <- data[-c(1,2,5,8,11,14,17,20),]
data1_1 <- data[c(1,3,5,7,9,11,13)]
data1_1
data1_2 <- data[c(2,4,6,8,10,12,14)]
data1_2

data1_1 <- as.numeric(data1_1)
data1_1
data1_2 <- as.numeric(data1_2)
data1_2

for(i in 1:length(data1_1)){
  data2 <- c(data2, data1_1[i], data1_2[i])
}
data2

barplot(data2, col=c("blue", "red"), main = "년도별 남녀 출생아수", legend.text = c("남", "여"))



