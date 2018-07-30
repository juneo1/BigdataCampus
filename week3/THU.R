## 7월 19일 오후 실습문제
# 5교시: (일원배치 분산분석)

#q1) iris의 품종(Species) 별로 Sepal.Width의 차이가 있는 지를 유의수준 5%에서 검정하여라. 
Fisher<-aov(Sepal.Width~Species, data=iris)
summary(Fisher)

#q2) <등분산성 검정> 품종별로 Sepal.Width의 분산들이 같다고 할 수 있는 가? 
bartlett.test(Sepal.Width~Species, data=iris)

#Ex.
pain= c(4,5,4,3,2,4,3,4,4,6,8,4,5,4,6,5,8,6,6,7,6,6,7,5,6,5,5)
drug= c(rep("A",9), rep("B",9), rep("C",9))
migraine = data.frame(pain, drug)
migraine

plot(pain ~ drug, data=migraine)

#q1)다음 프로그램을 구현한 후 결과를 해석하라.
results = aov(pain ~ drug, data=migraine)
summary(results)

#q2) 
pairwise.t.test(pain, drug, p.adjust="bonferroni")

#q3)
results = aov(pain ~ drug, data=migraine)
TukeyHSD(results, conf.level = 0.95)



# 6교시: (이원배치 분산분석)
#q1) Read in the data and inspect its structure.

#Read in the moth experiment data
setwd("c:/MyR")
moth.experiment = read.csv("moth-trap-experiment.csv", header = TRUE)

#Inspect structure of the data
head(moth.experiment)

#q2) Create summary statistics for location.
#get summary statistics for location group
library(psych)
describeBy(moth.experiment$number.of.moths,moth.experiment$location)

#q3) Create summary statistics for type of lure.
#get summary statistics for type of lure group
describeBy(moth.experiment$number.of.moths,moth.experiment$type.of.lure)

#q4) Create boxplots for each category.
#Create boxplots using the two factor variables
library(ggplot2)
ggplot(moth.experiment, aes(x=location,y=number.of.moths, fill = type.of.lure)) + geom_boxplot()

#q5) Check for normality.
#Check for normality of observations
shapiro.test(moth.experiment$number.of.moths)

#q6) Check for equality of variance.
library(car)
leveneTest(moth.experiment$number.of.moths~moth.experiment$location*moth.experiment$type.of.lure)


#q7) Take a log transformation of our data.
#take a log transformation of number of moths and check normality and equal variance
no.of.moth.log = log(moth.experiment$number.of.moths)
moth.experiment$no.of.moth.log = no.of.moth.log
shapiro.test(moth.experiment$no.of.moth.log)
leveneTest(moth.experiment$no.of.moth.log~moth.experiment$location*moth.experiment$type.of.lure)

  #the log transformation is not very effective in normalizing the data
  #the appropriate transformation is left as an exercise to the reader
  #this will help the reader appreciate challenges of analyzing data
  
#q8) Perform anova.
#perform anova 
moth.anova = aov(moth.experiment$no.of.moth.log~moth.experiment$location*moth.experiment$type.of.lure)
moth.anova

#when you have an unbalanced design R does not issue any warnings 
#to correctly analyze an unbalanced design we can use the Anova function in car library 
#we pass results of aov function and specify we would like to use Type III sums of squares 
library(car) 
Anova(moth.anova,type = "III")

#q9) 분산분석의 결과를 알기 쉽게 설명하라.
# location has an effect on number of moths. 
#type of lure does not have an effect on number of moths
#the combined effect of location and type of lure does not have an effect on number of moths


#7교시: (이원배치 분산분석)
grade <- c(71,77,78,76,77,78,71,70,69,76,76,80,79,78,77,71,71,70)
gender <- c(rep("M",9), rep("F",9))
class <- c(rep("Class 1",3),rep("Class 2",3),rep("Class 3",3),rep("Class 1",3),rep("Class 2",3),rep("Class 3",3))
statics <- data.frame(gender, class, grade)
class(statics$gender)
head(statics)

describeBy(statics$grade,statics$class)
describeBy(statics$grade,statics$gender)

#q1) 주효과: 학급과 성별에 대한 유의성을 유의수준 5%로 각각 설명하여라. 
class.anova <- aov(statics$grade~statics$class)
summary(class.anova)
gender.anova <- aov(statics$grade~statics$gender)
summary(gender.anova)

#q2) 교호작용: 학급*성별에 대한 유의성을 유의수준 5%로 설명하여라. 
summary(aov(statics$grade~statics$gender*statics$class))

#q3) 프로파일 도표 (교호작용의 존재 여부를 그래프로 그린 그림)을 이용하여 교호작용의 존재 여부를 설명하여라.
interaction.plot(class, gender, grade) 
