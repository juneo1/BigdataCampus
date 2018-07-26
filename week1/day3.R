# Paste : 문자열을 연결하는 함수
#,는  space sep을 사용하면 공백이 없음
paste("K", c(1:10))
paste("K", c(1:10), sep="")문 

#if문
a <-10

#반복문for
for(i in 1:10) {
  cat("2*", i, "=", 2*i, "\n")
}

for(i in c(2,5,10,20,50)) {
  cat(i, ": ", i^2, "\n", sep="")
}

for(i in c("진희", "미희", "건희", "동희")) {
  print(i)
}

for(i in seq(1,10,2)) {
  print(i) 
}


for(i in 2:50) {
  for(j in 2:i-1){
    if(i%%j == 0) {
      print(i)
    }
  }
}



#next(), break(), repeat
x <-1:100
for(i in x) {
  if (i %% 3 == 0) {
    next()
  }
  print(i)
}

x <- 0
repeat {
  x <- x + 1
  print(x)
  if(x > 100) break()
}

#p.86
# 문제1:100사이의 숫자중에서 짝수의 합과 개수를 홀수의 합과 개수를 각각 구하시오
even <- 0; ceven <- 0; 
odd <- 0; codd <- 0;

for(i in 1:100) {
  if (i%%2 == 0) {
    even <- even + i
    ceven <- ceven + 1
  } else {
    odd <- odd + i
    codd <- codd + 1
  }
}

cat("evensum : " , even , "evencount : " , ceven , "\n")
cat("oddsum : ", odd, "oddcount: ", codd)


# 문제2. 1:100 사이의 숫자 중에서 3의 배수이면서, 6의 배수인 수를, 6의 배수이면 6의 배수를,
# 3의배수면 3의 배수를 출력하시오

for(i in 1:100) {
  if (i %% 6 ==0){
    if(i%% 3 == 0) {
      cat(i, ": 3의배수이면서 6의배수 \n")
    }
  } else {
    if(i%%3 ==0) {
    cat(i, ": 3의배수 \n")
  }
 }
}

# 문제3: iris 데이터셋에서 Sepal.Width의 값이 4이상인 것이 몇개인지 출력한다.

ds <-  subset(iris, Sepal.Width >= 4.0)
count <- 0
for (i in 1:nrow(ds)){
  count = count + 1
}
print(count)




# 문제4: 데이터셋iris 출력결과에서 Sepal.Width값이 4보다 크면 "big"을 그렇지 않으면 "small"로 수정


IR <- iris
for(val in 1:nrow(IR)){
  if(IR[val,"Sepal.Width"] > 4) {
    IR[val,"Sepal.Width"] <- "big"
  } else {
    IR[val,"Sepal.Width"] <- "small"
  }
}
IR


# 문제5: iris 데이터셋에서 아래 보기의 Species별Petal.Width의 평균이다 . for문을 이용한다.

IR <- iris
count <- 0 ; vcount <- 0; vcount2 <- 0
setosa.pw <- 0 ; versicolor.pw <- 0; virginica.pw <- 0;
for(val in 1:nrow(IR)) {
  if(IR[val,"Species"] == "setosa") {
      setosa.pw <- setosa.pw + IR[val, "Petal.Width"]
      count <- count + 1 
  } 
  if(IR[val,"Species"] == "versicolor") {
      versicolor.pw <- versicolor.pw + IR[val, "Petal.Width"]
      vcount <- vcount + 1 
  }
  if(IR[val,"Species"] == "virginica") {
      virginica.pw <- virginica.pw + IR[val, "Petal.Width"]
      vcount2 <- vcount2 + 1 
    }
 }
print(setosa.pw/count)
print(versicolor.pw/vcount)
print(virginica.pw/vcount2)


#while문 : 시작값이 정해지지 않아서 while문 밖에 시작값이 있고 아래 조건이 있다.
#repeat(): 무한반복, 끝나는 값은 break을 걸어준다.
x <- 0
repeat {
  x <- x+1
  print(x)
  if(x>100) break()
}



#사용자 정의함수

a <- function() {
  x <- 0
  repeat {
    x <- x+1
    print(x)
    if(x>100) break()
  }
}

# mymax
mymax <- function(x,y) {
  num.max <- x
  if(y > x) {
    num.max <- y
  }
  return (num.max)
}
mymax(10,15)
mymax(20,15)

# totalsum
total <- function(x,y) {
  sum <- 0
  for(x in x:y) {
    sum <- sum + x
  }
  return(sum)
}
total(1,10)
total(5,10)

#문제1 : iris n열의 평균
ir <- function(x) {
  IR <- iris[, -5]
  avg <- 0
  for(i in 1:nrow(IR)) {
    avg <- avg + IR[i,x]
  }
  cat (x, "열의 평균 : ")
  return (avg/nrow(IR))
}

ir(2)

#문제2 : 함수실행시 입력된 값의 범위내에서 7의배수의 개수를 구할 수 있다.
cnttot <- function(x,y) {
  cnt <- 0
  for(i in x:y) {
    if (i%%7 == 0) {
      cnt <- cnt+1
    }
  }
  return (cnt)
}
cnttot(1,50)
cnttot(1,4000)


#화면에서 사용자 입력 값 받기
n <- readline(prompt="숫자를 입력하세요: ")
cat("입력한 숫자는", n, "입니다. \n")
class(n)
as.numeric(n)
class(n)


#문제5: 함수 실행시 임의의 숫자를 입력하면 1부터 입력범위의 합을 구한다.
total <- function() {
  n <- readline(prompt="숫자를 입력하세요: ")
  
}

#문제6: 임의의 숫자 10개를 입력받아 최대값을출력한다
ranmax <- function() {
  max <- -999
  for(i in 1:10) {
    num <- readline(prompt="숫자를 입력하세요: ")
    num <- as.numeric(num)
    if(num > max) {
      max <- num
    }
  }
  return (max)
}
ranmax()


#p.93 문제
#1. 1~100사이의 숫자중 3의 배수를 출력하는 프로그램을 작성하시오
for(i in 1:100) {
  if(i%%3==0) {
    print(i)
  }
}

#2. 101~200사이의 숫자중 3과 4의 공배수를 출력하는 프로그램을 작성하시오
for(i in 101:200) {
  if(i%%12 ==0) {
    print(i)
  }
}

#3. 24의 약수를 출력하는 프로그램을 작성하시오
for(i in 1:24) {
  if(24%%i == 0) {
    print(i)
  }
}

#4. 10!을 출력하는 프로그램을 작성하시오
rst <- 1
for(i in 10:1){
  rst = rst * i
}
print(rst)
# factorial(10)

#5. 세개의 숫자를 매개변수로 입력하면 그중에 가장 큰수를 돌려주는 함수를 작성하고 테스트하시오
findmax <- function(x,y,z){
  max <- x
  if(x < y){
    max <- y
    if( y < z) {
      max <- z
    }
  }
  if (x < z) {
    max <- z 
    if(z < y) {
      max <- y
    }
  }
  return(max)
}


findmax(1,4,5)
findmax(4,1,3)
findmax(2,6,7)

#6. 화면에서 숫자 2개를 입력 받아 두 숫자의 합과 곱을 출력하는 프로그램을 작성하시오
calc <- function() {
  while(TRUE){
    fnum <- readline(prompt="첫번 째 숫자를 입력하세요: ")
    snum <- readline(prompt="두번 째 숫자를 입력하세요: ")
    
    if (fnum==0 & snum==0) {
      break
    } else{
      plus <- as.numeric(fnum) + as.numeric(snum)
      multi <- as.numeric(fnum) * as.numeric(snum)
      cat(fnum, "+", snum, "=",  plus, "\n")
      cat(fnum, "*", snum, "=",  multi, "\n")
    }
  }
}
calc()

#7.한 사람이 주민등록번호를 입력하여 생년월일과 남,여 여부를 출력하는 프로그램을 작성하시오
dat = readline("insert your joomin!")

dat = strsplit(dat, "-")[[1]]
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

#8. 데이터의 개수가 정해지지 않으면서 데이터가 임의로 입력될 때 데이터의 합계, 평균, 최대, 최소값의 
#   위치를 계산하는 프로그램을  for반복문을 이용하여 각각 작성하라. 데이터 맨 마지막은 임의로 9999입력

num <- c()
while(TRUE) {
  a = readline("data: ")
  if (a == "9999") {
    break()
  }else{
    a = as.numeric(a)
    num = c(num,a)
  }
}
mean(num)
max(num)
min(num)
sum(num)


#9. total이라는 함수로 1부터 10까지
value = 0
for(i in 1:10){
  for(j in 1:i){
    value = value + j
  }
}
value

#10. 벡터에 10개의 데이터가 입력되어 있다. 데이터에서 중복되는 데이타들의 수를 출력한다.

dat= c(1,2,1,1,3,2,6)
unique(dat)
for(i in unique(dat)){
  d = sum(dat %in% unique(dat)[1])
}


# apply함수

# 막대그래프 작성
favorite.color <- c("red", "green","yellow", "red", "green", "red", "red")
sum <- table(favorite.color)
sum
barplot(sum, main="Favorite color")


#color막대그래프
height <- c(9,15,20,6)
name <- c("판매1팀", "판매2팀", "판매3팀", "판매4팀")
barplot(height, names.arg = name, xlab = "판매팀", ylab="판매실적(억)", main="부서별 판매 실적",
        col=rainbow(length(height)))

#원 그래프 
favorite.color <- c("red", "green", "yellow", "red", "green", "red", "red")
sum <- table(favorite.color)
pie(sum, main="Favorite color")