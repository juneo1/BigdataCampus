

#Final 1.
sex <- c("��","��","��","��","��","��")
height <- c(190,167,180,167,175,175)
weight <- c(67,65,70,74,67,85)
data <- data.frame(sex,height,weight)
#View(data)
data

#2.
rownames(data)<-c("�嵷��", "������", "������", "�ڸ��", "���ڼ�", "��ȣ��")
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
    st[i] <- "��ü��"
  } else if(data[i,"BMI"] >= 25.0 & data[i,"BMI"] < 30) {
    st[i] <- "��ü��"
  } else if(data[i,"BMI"] >= 30) {
    st[i] <- "��"
  } else {
    st[i] <- "����"
  }
}
data <- cbind(data,st)
data

#5. 
data1 <- data[,5]
barplot(table(data1), ylab = "�ο���", col=rainbow(2))

#6. 
boxplot(BMI~sex, data <-  data, xlab="����", ylab="BMI")

#7.
