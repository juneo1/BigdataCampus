iris
is.data.frame(iris)
iris[, "Species"]


## iris$Species => 결과가벡터matrix에서는 사용하지 않음

nrow(iris)

dim(iris)
ncol(iris)
names(iris)
head(iris)
tail(iris)
head(iris, n=10)

####



colSums(iris[,-5])
colMeans(iris[,-5])
rowSums(iris[,-5])
rowMeans(iris[,-5])



z <- matrix(1:20, nrow=4, ncol=5)
z
t(z)
colnames(z) <- c("Korean", "Math", "English", "Social Science", "Physics")
rownames(z) <- c("Kim", "Park", "Choi", "Seo")
z

 #####

IR.1 <- subset(iris, Species=="setosa")
IR.1

###Matrix 연산

a <- matrix(1:20, 4, 5)
b <- matrix(21:40, 4, 5)
a
b
a+b
b-a
b/a
a*b

3*a


#### Matrix vs Data frame
class(iris)
class(state.x77)
is.matrix(iris)

state.x77

iris.m <- as.matrix(iris[,1:4])
iris.m

head(iris.m)


st <- data.frame(state.x77,state.region)
st




##  Q.아래보기처럼 수정해본다. 5열의 품종명이 숫자로 표기되어 있다.

iris
IR.1 <-  subset(iris, Species == "setosa")
N.1 <- c(1)
IR.2 <-  subset(iris, Species == "versicolor")
IR.3 <-  subset(iris, Species == "virginica")

IR <- data.frame (IR.1, N.1)
IR

# 5칼럼만 떼서 형변환하고 다시 붙이기

ta <- iris[,5]
ta
he <- iris[, -5]
he
class(ta)
class(he)
ta <- as.numeric(ta)
st <- cbind(he, ta)
st


## 파일에 데이터 읽기/쓰기

setwd("D:/Rwork")
mydata <-  read.csv("test.csv", header = T)
mydata
head(mydata)
tail(mydata)

mydata[2,3]  # 2행 3열의 원소값 출력
nrow(mydata) # 행의 개수 출력
ncol(mydata) # 열의 개수 출력
dim(mydata) # 행, 열의 개수 출력

myRow1 <- mydata[2,]
myCol <- mydata[,3]
myRow <- mydata[3]
myRow
myRow1
myCol


mynew <- mydata[,c(2,3)]
write.csv(mynew, "Kid_new.csv", row.names = F, quote = F)

mydata <- read.csv(file.choose(), header=aT)

#R에서 제공되는 Dataset 리스트 조회
library(help="datasets")
data()


#  petal.Length이 1.5이상인 것만 추출해서 저장하기

mydata <- subset(iris, Petal.Length >= 1.5)
write.csv(mydata, "iris.Petal.csv")
read.csv("iris.Petal.csv")


# R의dataset 에서 quakes데이터 앞의 10줄만 파일에 저장하기

myquakes <-  head(quakes, n=10)
write.csv(myquakes, "quakes.csv")
read.csv("quakes.csv")


# 웹의 데이터파일 열기
url <- "http://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv"
x <- read.csv(url)
x


nrow(x)
survive <- subset(x, survived == "yes")
survive


##

a <- nrow(subset(x, survived == "yes"))
str(x)
a <- sum(x[,5])



## 통계청 데이터 이용하기   

read.csv("kostat.csv")



##

#ds <- read.csv("PC.csv", fileEncoding="euc-kr")
ds <- read.csv("PCbangTrics.csv")
View(ds)

table(ds[,"게임장르"])  #table(ds[,4])
ta <- subset(ds, 게임장르== "레이싱")

#가장 많은 장르 tmax에서 최대값
ta1 <- table(ds[,4])
tmax <- max(ta1)
tmax
taframe <- data.frame(ta)
taframe
subset(taframe, taframe$Freq=tmax)




### package 사용

library(ggplot2)
scatter <-  ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))
scatter + geom_point(aes(color=Species, shape=Species)) + xlab("Sepal Length") + ylab("Sepal Width") + ggtitle("Sepal Lenth-Width")



##오후수업
head(subset(iris, Species == "setosa"))
subset(x=iris, subset = (Species =="setosa"), select = )
subset(x= )
?subset


#p.78 연습문제
class(state.x77)

#1.state.x77을 st에 data.frame 으로 저장하시오
st <- data.frame(state.x77)  

#2. st의 내용을 보이시오
st

#3.st의 열 이름을 보이시오
colnames(st)

#4. st의 행이름을 보이시오
rownames(st)

#5. st의 행의 개수와 열의 개수를 보이시오
dim(st) ; nrow(st); ncol(st)

#6. st의 요약정보를 보이시오 
str(st) ; summary(st)

#7. st의 행별 합계와 평균을 보이시오
rowSums(st)
rowMeans(st)
#sum(st$Population)

#8. st의 열별 합계와 평균을 보이시오
colSums(st)
colMeans(st)

#9. Florida주의 모든 정보를 보이시오
subset(st,colnames(st)== "Florida")
st["Florida", ]

#10. 50개 주의 Income 정보만 보이시오
st[, c("Income")]
st[, "Income"]
st$Income

#11. texas주의 면적을 보이시오
st["Texas","Area"]

#12. ohio주의 인구와 수입을 보이시오
subset(st, rownames(st)=="Ohio", select =c(Population, Income))
st["Ohio", c("Population", "Income")]


#13. 인구가 5000이상인 주의 데이터만 보이시오
subset(st, Population >= 5000)
st[st$Population > 5000, ]

#14. 수입이 4500 이상인 주의 인구, 수입, 면적을 보이시오
subset(st, Income>= 4500, select = c(Population, Income, Area))
subset(x=st, subset=Income>=4500, select = c("Population", "Income", "Area"))
st[st$Income >= 4500, c("Population", "Income", "Area")]

#15. 수입이 4500이상인 주는 몇개인지 보이시오
nrow(subset(st, Income>4500))

#16. 전체면적이 100000이상이고 결빙일수가 120이상인 주의 정보를 보이시오
subset(st, Area>=100000 & Frost >=120)

#17.문맹률이 2.0이상인 주의 평균수입은 얼마인가
colMeans(subset(st, Illiteracy>=2.0, select = Income))
colMeans(subset(st, Illiteracy>=2.0, "Income"))

#18. 문맹률이 2.0 미만인 주와 2.0 이상인 주의 평균 수입차이를 보이시오
colMeans(subset(st, Illiteracy<2.0, "Income"))
colMeans(subset(st, Illiteracy>=2.0, select = Income))
colMeans(subset(st, Illiteracy<2.0, select = Income)) - colMeans(subset(st, Illiteracy>=2.0, select = Income))

#19. 기대수명이 가장 높은주는 어디인가
subset(st, Life.Exp == max(st$Life.Exp))
rownames(subset(st, Life.Exp == max(st$Life.Exp)))

#20.  Pennsylvania보다 수입이 높은 주들을 보이시오
rownames(subset(st, Income > st["Pennsylvania", "Income"]))


#####SQL 처리

install.packages("sqldf")
library(sqldf)



#24.
st <- data.frame(state.x77)
rich_state <- subset(st, Income>=5000)
write.csv(rich_state, "rich_state.csv")

#풀이
ds = st[st$Income >= 5000, ]
write.csv(x = ds, file = "rich_state.csv")
getwd()

#25.
ds <- read.csv("rich_state.csv")
ds


## 조건문 If문

a <- 10
if(a > 5) {
  print (a)
} else {
  print (a*10)
  print (a/10)
}


a <- 10
b <- 20
ifelse(a>b, c<-a, c<-b)
c

## 반복문 for
for(i in 1:10) {
  print(i)
}

for(i in 1:10) {
  cat("2*", i, "=", 2*i, "\n")
}

for(i in 1:20) {
  if(i%%2==0) {
    print(i)
  }
}

sum <- 0
for(i in 1:100) {
  sum <- sum + i 
}
print (sum)


## 반복문 while
i <- 1
while(i <= 10) {
  print(i)
  i <- i+1
}


## 사용자정의 함수 만들기

mymax <- function(x,y) {
  num.max <- x
  if(y > x){
    num.max <- y
  }
  return (num.max)
}

mymax(10,15)
mymax(20,15)

mydiv <- function(x,y=2) {
  result <- x/y
  return(result)
}

##화면에서 사용자 입력값 받기
n <- readline(prompt="숫자를 입력하세요: ")
cat("입력한 숫자는 ", n, "입니다. \n")


## 2중 for문으로 구구단 만들기
for(i in 2:9) {
  cat("<", i, "단 > \n" )
  for(j in 1:9) {
    cat(i,"*",  j, "=", i*j, "\n")
  }
}
