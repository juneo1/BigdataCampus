

#Final 1.
sex <- c("남","여","여","남","남","남")
height <- c(190,167,180,167,175,175)
weight <- c(67,65,70,74,67,85)
data <- data.frame(sex,height,weight)
#View(data)
data

#2.
rownames(data)<-c("장돈건", "원비인", "이저인", "박면수", "유자석", "강호돈")
data

#3.
BMI <- c(0,0,0,0,0,0)
wdata <- data[,"weight"]
hdata <- data[,"height"]
wdata; hdata

for(i in 1:nrow(data)){
  #BMI[i] <- wdata[i] / (hdata[i]/100 * (hdata[i]/100))
  bmi <- wdata[i] / ((hdata[i]/100) * (hdata[i]/100))
  BMI[i] <- round(bmi,0)
}

data <- cbind(data, BMI)
data

#4. 
st <- c(0,0,0,0,0,0)


for(i in 1:nrow(data)){
  if(data[i,"BMI"] <= 18.5){
    st[i] <- "저체중"
  } else if(data[i,"BMI"] >= 25.0 & data[i,"BMI"] < 30) {
    st[i] <- "과체중"
  } else if(data[i,"BMI"] >= 30) {
    st[i] <- "비만"
  } else {
    st[i] <- "정상"
  }
}
data <- cbind(data,st)
data

#5. 
data1 <- data[,5]
barplot(table(data1), ylab = "인원수", col=rainbow(2))

#6. 
boxplot(BMI~sex, data <-  data, xlab="성별", ylab="BMI")

#7.

