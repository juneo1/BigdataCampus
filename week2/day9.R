setwd("D:/Rwork")

ds <- read.csv("프로젝트1.csv",  stringsAsFactors = FALSE)
ds <- ds[-c(1:2),1]
colnames(ds) <- c("회차", "추첨일", "1등당첨자수","1등당첨금액", "2등당첨자수", "2등당첨금액", "3등당첨자수", "3등당첨금액", 
                  "4등당첨자수", "4등당첨금액", "5등당첨자수", "5등당첨금", "번호1", "번호2", "번호3", "번호4", "번호5", "번호6", "보너스번호")

num1.ds <- ds[, "번호1"]
num2.ds <- ds[, "번호2"]
num3.ds <- ds[, "번호3"]
num4.ds <- ds[, "번호4"]
num5.ds <- ds[, "번호5"]
num6.ds <- ds[, "번호6"]

num.ds <- ds[, 13:18]
View(num.ds)

count <- c(1:45)
for(i in 1:45) {
  count[i] <-  0
}
count

table(num1.ds)
table(num2.ds)
table(num3.ds)
table(num4.ds)
table(num5.ds)

for(i in 1:nrow(ds)) {
  for(j in 1:6) {
    for(k in 1:45) {
      if(num.ds[i,j] == k) {
        count[k] <-  count[k]+1
      }
    }
  }
}

View(count)
names(count) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
                  "40", "41", "42", "43", "44", "45")
s.count <- sort(count, decreasing = T)
View(s.count)

#barplot(s.count[1:10], xlab="번호", ylab="빈도수", col=rainbow(length(s.count[1:10])))
barplot(s.count[1:10], main="로또 최다 빈출번호", xlab="번호", ylab="빈도수", col="blue")


### 8조
#엑셀 파일 csv로 변환 후 'project.csv'로 내문서에 저장, getwd()로 실행경로 확인후 실행하세요

lotto <- read.csv('프로젝트1.csv',stringsAsFactors = F)

lotto <- lotto[-c(1,2),-1]
colnames(lotto) <- c('Time','Date','Rank.1.pop','Rank.1.price','Rank.2.pop','Rank.2.price','Rank.3.pop','Rank.3.price',
                     'Rank.4.pop','Rank.4.price','Rank.5.pop','Rank.5.price',
                     'Lot.num.1','Lot.num.2','Lot.num.3','Lot.num.4','Lot.num.5','Lot.num.6','Bonus.num') 
str(lotto)

lotto$Rank.1.price <- gsub('원','',lotto$Rank.1.price)
lotto$Rank.1.price <- gsub(',','',lotto$Rank.1.price)

lotto$Rank.2.price <- gsub('원','',lotto$Rank.2.price)
lotto$Rank.2.price <- gsub(',','',lotto$Rank.2.price)

lotto$Rank.3.price <- gsub('원','',lotto$Rank.3.price)
lotto$Rank.3.price <- gsub(',','',lotto$Rank.3.price)
lotto$Rank.3.pop <- gsub(',','',lotto$Rank.3.pop)

lotto$Rank.4.price <- gsub('원','',lotto$Rank.4.price)
lotto$Rank.4.price <- gsub(',','',lotto$Rank.4.price)
lotto$Rank.4.pop <- gsub(',','',lotto$Rank.4.pop)

lotto$Rank.5.price <- gsub('원','',lotto$Rank.5.price)
lotto$Rank.5.price <- gsub(',','',lotto$Rank.5.price)
lotto$Rank.5.pop <- gsub(',','',lotto$Rank.5.pop)

lotto$Date <- gsub('\\.','-',lotto$Date)


str(lotto)

lotto$Time <- as.integer(Time)
lotto$Date <- as.Date(Date)

lotto$Rank.1.pop <- as.numeric(lotto$Rank.1.pop)
lotto$Rank.1.price <- as.numeric(lotto$Rank.1.price)

lotto$Rank.2.pop <- as.numeric(lotto$Rank.2.pop)
lotto$Rank.2.price <- as.numeric(lotto$Rank.2.price)

lotto$Rank.3.pop <- as.numeric(lotto$Rank.3.pop)
lotto$Rank.3.price <- as.numeric(lotto$Rank.3.price)

lotto$Rank.4.pop <- as.numeric(lotto$Rank.4.price)
lotto$Rank.4.price <- as.numeric(lotto$Rank.4.price)

lotto$Rank.5.pop <- as.numeric(lotto$Rank.5.pop)
lotto$Rank.5.price <- as.numeric(lotto$Rank.5.price)

lotto$Lot.num.1 <- as.integer(lotto$Lot.num.1)
lotto$Bonus.num <- as.integer(lotto$Bonus.num)

str(lotto) 



install.packages('lubridate')
library(lubridate)

#년별, 월별, 년월별 1등 금액평균
date.lotto <- lotto[, c(2,4)]
y <- cbind(date.lotto, year=as.numeric(substr(date.lotto$Date, 1, 4)))

mean <- c(2002:2018)
count <- 0
for(i in min(y$year):max(y$year)){
  count <- count + 1
  for(j in 1:nrow(y)){
    if(y[j, "year"] == i){
      mean[count] <- mean[count] + y[j, "Rank.1.price"]
    } 
  }  
}

ycnt <- table(y$year)

for(i in 1:17){
  mean[i] <- mean[i]/(ycnt[i]*10000000)
}


names(mean) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                 "2013", "2014", "2015", "2016", "2017", "2018")
barplot(mean, main="년도별 평균 1등당첨금액", xlab="Year", ylab="단위(천만원)")

# 월별 1등 금액평균
m <- cbind(date.lotto,month=month(date.lotto$Date))
mmean <- c(1:12)
count <- 0
for(i in min(m$month):max(m$month)){
  count <- count + 1
  for(j in 1:nrow(m)){
    if(m[j, "month"] == i){
      mmean[count] <- mmean[count] + m[j, "Rank.1.price"]
    } 
  }  
}

mcnt <- table(m$month)
mcnt
for(i in 1:12){
  mmean[i] <- mmean[i]/(mcnt[i]*10000000)
}

names(mmean) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
barplot(mmean, main="월별 평균 1등당첨금액", xlab="Month", ylab="단위(천만원)")
#plot(mmean,type="l", main="월별 평균 1등당첨금액", xlab="Month", ylab="단위(천만원)")

#1등 당첨수 평균
stavg.lotto <- lotto$Rank.1.pop
View(stavg.lotto)
lotto.mean <- mean(stavg.lotto)
lotto.mean




#최다빈출번호
View(lotto)

num.ds <- lotto[, 13:18]
num.ds

count <- c(1:45)
for(i in 1:45) {
  count[i] <-  0
}
count

for(i in 1:nrow(num.ds)) {
  for(j in 1:6) {
    for(k in 1:45) {
      if(num.ds[i,j] == k) {
        count[k] <-  count[k]+1
      }
    }
  }
}

View(count)
names(count) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
                  "40", "41", "42", "43", "44", "45")
s.count <- sort(count, decreasing = T)
View(s.count)

barplot(s.count, main="로또 최다 빈출번호", xlab="번호", ylab="빈도수(회)", col="gray")
barplot(count, main="로또 최다 빈출번호", xlab="번호", ylab="빈도수(회)", col="gray")
hist(count)

# 년월도별 당첨숫자

year.data <- year(date.lotto$Date)
month.data <- month(date.lotto$Date)
yearmonth.data <- paste(year.data,'.', month.data, sep = '') 


yearmonth.data <- cbind(date.lotto, yearmonth.data)
View(yearmonth.data)

yearmonth.mean <- tapply(yearmonth.data$Rank.1.price,yearmonth.data$yearmonth.data,mean)
barplot(yearmonth.mean)

ym.data <- cbind(lotto, yearmonth.data)
ym.data

#



#
avgwin = mean(lotto$Rank.1.price) + mean(lotto$Rank.2.price) + mean(lotto$Rank.3.price) + mean(lotto$Rank.4.price) + mean(lotto$Rank.5.price)
avgwin

avgpnum <- c(1:45)
avgsum <- sum(count)
avgsum

for(i in 1:45) {
  avgpnum[i] <- avgwin*(count[i]/avgsum) 
}

avgpnum

barplot(avgpnum)


## 로또번호 뽑는 단계

prob <- c(1:45)
for(i in 1:45) {
  prob[i] <- count[i]/sum(count)
}

# prob: 지금까지의 시행했던 자료의 해당번호의 확률

pick <- function() {
  data <- c()
  dat <- readline("생성할 게임수?")
  dat <- as.integer(dat)
  #sample <- c(1:dat) 
  for(i in 1:(dat-1)){
    makelot <- sample(1:45,7,prob=prob)
    data <- rbind(data, makelot)
  }
  
  tempprob <- c(1:45)
  tempcount <- c(1:45)
  for(i in 1:45) {
    tempcount[i] <-  0
  }
  tempcount
  for(i in 1:length(data)){
    for(j in 1:45){
      if(data[i] == j)
        tempcount[j] <- tempcount[j]+1  
    }
  }
  names(tempcount) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                        "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
                        "40", "41", "42", "43", "44", "45")
  
  for(i in 1:45){
    tempprob[i] = tempcount[i]/sum(tempcount)
  }
  
  makelot <- sample(1:45,7,prob=tempprob)
  data <- rbind(data, makelot)
  
  print(data)
} 

tempprob
pick()


### 연습
data <- c()
for(i in 1:9){
  makelot <- sample(1:45,7,prob=prob)
  data <- rbind(data, makelot)
  
}
data


tempprob <- c(1:45)
tempcount <- c(1:45)
for(i in 1:45) {
  tempcount[i] <-  0
}
tempcount
for(i in 1:length(data)){
  for(j in 1:45){
    if(data[i] == j)
    tempcount[j] <- tempcount[j]+1  
  }
}
names(tempcount) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
                  "40", "41", "42", "43", "44", "45")
  
for(i in 1:45){
  tempprob[i] = tempcount[i]/sum(tempcount)
}

makelot <- sample(1:45,7,prob=tempprob)
data <- rbind(data, makelot)

