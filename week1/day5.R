#7월 6일 다변량 자료 탐색

## 산점도(Scatter Plot)
head(mtcars)
wt <- mtcars$wt
mpg <- mtcars$mpg
plot(wt~mpg, main=" Car Weight-mpg", xlab="Car Weight", ylab = "Miles per Gallon", col="red", pch=19)
plot(wt, mpg, main=" Car Weight-mpg", xlab="Car Weight", ylab = "Miles per Gallon", col="red", pch=19)

## pairs(): 여러 변수들 사이의 상관관계를 한번에 확인
vars <- c("mpg", "disp", "drat", "wt")  #대상변수
target <- mtcars[,vars]
pairs(target, main="Multi plots") #대상 데이터


## 그룹 정보가 있는 2변량 데이터의 분포 보기
iris.2 <- iris[,3:4] #데이터
point <- as.numeric(iris$Species)
color <- c("red", "green", "blue")
plot(iris.2, main="Iris plot", pch=c(point), col=color[point])


## [연습1]
#1. cars데이터셋의 speed와 dist에 대한 산점도를 그리시오
spd <- cars$speed
dist <- cars$dist
plot(spd, dist, main="Dist-Speed", xlab="Speed", ylab="Dist", col="blue", pch=20)
#==> 속도가 빠를수록 제동거리가 길다

#2.pressure데이터셋을 이용해서  temperature와 pressure에 대한 산점도를 그리시오
head(pressure)
temp <- pressure$temperature
press <- pressure$pressure
plot(temp, press, main="Press-Temperature", xlab="Tempature", ylab="Pressure", col ="red", pch=9)

#3.  state.x77데이터셋에서 population, Income, Illiteracy, Area 변수간 산점도를 그린다.
head(state.x77)
st <- state.x77[,c(1,2,3,8)]
pairs(st,main="state.x77 multi plots")

#4. iris 데이터셋에서 Species 정보에 따른 Sepal.Length , Sepal.Width의 분포를 알아보시오
iris
ir <- iris[, 1:2]
point <- as.numeric(iris$Species)
color <- c("red", "green", "blue")
plot(ir, main="Iris plot", pch=c(point), col=color[point])

#5. women데이터셋의 키와 몸무게와의 상관관계
head(women)
plot(women$height, women$weight, main="women height-weight plot", xlab="weight", ylab= "height", col="red", pch=7)

#6.iris 데이터셋을 4가지 속성으로 pairs
pairs(iris[, 1:4], main="3 Species", pch=21, bg=c("red", "green3", "blue"))


## 상관분석
beers <- c(5,2,9,8,3,7,3,5,3,5)
bal <- c(0.1, 0.03, 0.19, 0.12, 0.04, 0.095, 0.07, 0.06, 0.02, 0.05)
table <- data.frame(cbind(beers,bal))
plot(bal~beers, data=table) #산점도
res <- lm(bal~beers,data=table) #회귀식도출
abline(res) #회귀선 그리기
cor(beers, bal)

## 연습2
#1.
income <- c(125000, 100000, 40000, 35000, 41000, 29000, 35000, 24000, 50000, 60000)
years <- c(19,20,16,16,18,12,14,12,16,17)
table <- data.frame(cbind(income,years))
plot(table)
cor(income, years)


#2.
gpa <- c(3.1, 2.4, 2.0, 3.8, 2.2, 3.4, 2.9, 3.2, 3.7, 3.5)
hours <- c(14, 10, 20, 7, 25, 9, 15, 13, 4, 21)
plot(gpa, hours, col="blue", pch=20)
cor(gpa, hours)

#3. mtcars데이터셋에서  mpg와 다른변수들간이 상관계수를 구하시오.
head(mtcars)
pairs(mtcars[,1:6])
cor(mtcars[,1:8])

#4. iris데이터셋에서 Sepal.width가 다른변수들과의 상관관계를 산점도와 상관계수를 통해 구해본다.
plot(iris[, 1:4])
cor(iris[,1:4])


## 선그래프
month <- 1:12
late <- c(5,8,7,9,4,6,12,13,8,6,6,4)
plot(month,late,type="l")

month <- 1:12
late1 <- c(5,8,7,9,4,6,12,13,8,6,6,4)
late2 <- c(4,6,5,8,7,8,10,11,6,5,7,3)
plot(month,late1,type="l", col="red")
lines(month,late2,type="b",col="blue")


##연습3.
#1. 
years <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026)
val <- c(51014,51245,51446,51635,51811,51973,52123,52261,52388,52504,52609,52704)
plot(years, val, ylab = "인구수(천명)", type="l", col="blue")

#2. 
years <- c(20144, 20151, 20152, 20153, 20154, 20161, 20162, 20163, 20164, 20171, 20172, 20173)
male <- c(73.9, 73.1, 74.4, 74.2,  73.5, 73.0, 74.2, 74.5, 73.8, 73.1, 74.5, 74.2)
fmale <- c(51.4, 50.5, 52.4, 52.4, 51.9, 50.9, 52.6, 52.7, 52.2, 51.5, 53.2, 53.1)
names(years) <- c("2014년 4분기", "2015년 1분기", "2015년 2분기", "2015년 3분기", "2015년 4분기", "2016년 1분기",
             "2016년 2분기","2016년 3분기", "2016년 4분기", "2017년 1분기", "2017년 2분기", "2017년 3분기")
plot(male,type="l",col="red",main="남녀 경제활동참가율", ylim = c(50,80), axes=F)
lines(fmale,type="l",col="blue")
axis(1, 1:12, names(years))
axis(2, 40:90)



### 서울 중심 지도 출력
install.packages("ggmap")
library(ggmap)
library(ggplot2)

gc <- geocode(enc2utf8("성남"))
cen <- as.numeric(gc)
map <- get_googlemap(center=cen, maptype = "terrain", markers = gc)
# options available are "terrain", "satellite", "roadmap", and "hybrid"
ggmap(map,extent="device")

###  단양8경 지도

names <- c("1.도담삼봉/석문", "2.구담/옥순봉", "3.사인암", "4.하선암", "5. 충선암", "6.상선암")
addr <- c("충청북도 단양군 매포읍 삼봉로 644-33",
          "충청북도 단양군 단성면 월악로 3827", 
          "충청북도 단양군 사인암2길 42",
          "충청북도 단양군 단성면 선암계곡로 1337",
          "충청북도 단양군 단성면 선암계곡로 868-2",
          "충청북도 단양군 단성면 선암계곡로 798")

gc <- geocode(enc2utf8(addr))
gc

df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
df

cen <- c(mean(df$lon), mean(df$lat))
cen

map <- get_googlemap(center=cen, maptype = "roadmap", zoom = 16 markers = gc)
ggmap(map)




### 용인시에 존재하는 스타벅스 매장 3개정도 출ㄹ

names <- c("스타벅스 용인기흥구청점", "스타벅스 강남대점", "스타벅스 용인동백DT점")
addr <- c("경기도 용인시 기흥구 구갈로 71-18",
          "경기도 용인시 기흥구 강남로 3",
          "경기도 용인시 기흥구 어정로 102")

gc <- geocode(enc2utf8(addr))
gc


df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
df

cen <- c(mean(df$lon), mean(df$lat))
cen

map <- get_googlemap(center=cen, maptype = "roadmap", zoom = 14, markers = gc)
ggmap(map)




### 텍스트마이닝과 워드 클라우드
install.packages("wordcloud")
library(wordcloud)

word <- c("인천광역시","강화군", "옹진면")
frequency <- c(651,85,61)
wordcloud(word,frequency, colors = "blue")


### 텍스트마이닝 2
install.packages("KoNLP")
install.packages("RColorBrewer")

library(wordcloud)
library(KoNLP)
library(RColorBrewer)
useSejongDic()

pal2 <- brewer.pal(8,"Dark2")
text <- readLines(file.choose())

noun <- sapply(text,extractNoun, USE.NAMES = F)
noun

noun2 <- unlist(noun)
noun2

word_count <- table(noun2)
word_count

head(sort(word_count, decreasing = TRUE), 10)

wordcloud(names(word_count), freq = word_count, scale=c(6,0.2), min.freq=3, random.order = F,
          rot.per = .1, colors = pal2)


