### 7월 16일 실습문제
### 5교시: (실습을 통해서 개념 완성)

# q1) iris$Sepal.Width에 대해 상자그림을 그려라.
str(iris)
iris$Sepal.Width
q1 <- boxplot(iris$Sepal.Width)

# q2) q1의 결과를 이용하여 해석한 다음 내용 중 틀린 부분을 지적하라.
q1


# q3) iris의 setosa종과 versicolor종의 Sepal.Width에 대한 상자 그림을 그린 뒤 이 두 종의 중앙값이 다른지를 다음 프로그램을 이용하여 설명하여라.
sv <- subset(iris, Species =="setosa" | Species =="versicolor")
sv
sv$Species <- factor(sv$Species)
sv$Species
boxplot(Sepal.Width ~ Species, data=sv, col="gray", notch=TRUE)

# q4) Y-축을 도수로 표시한 iris$Sepal.Width에 대해 히스토그램을 그려라
hist(iris$Sepal.Width, 
     main="Histogram of Sepal.Width with Frequnecy", 
     col="lightgreen", 
     freq = T)

# q5) Y-축을 밀도로 표시한 iris$Sepal.Width에 대해 히스토그램을 그려라.
hist(iris$Sepal.Width, 
     main="Histogram of Sepal.Width with Density", 
     col="lightblue", 
     freq = F)

# q6) iris$Sepal.Width 데이터에 대한 커널밀도 추정그림을 그려라.
plot(density(iris$Sepal.Width),col='blue')

# q7) q5)의 결과에 밀도그림을 겹쳐서 그려라
hist(iris$Sepal.Width, 
     main="Histogram of Sepal.Width with Density", 
     freq = F)
lines(density(iris$Sepal.Width))

# q8) 다음 문장을 이용하여 Sepal.Width의 평균 값을 종별로 구하고 그 값을 막대 그림으로 나타내어라. 
# tapply는 ‘데이터, 그룹 인덱스, 각 그룹별로 호출할 함수’를 인자로 받는다.
barplot( tapply ( iris $ Sepal.Width , iris $ Species , mean ), col=rainbow(3))

### 6교시: (Guide to Create Beautiful Graphics in R)

# Installation
install.packages('ggplot2')

# Loading
library(ggplot2)


# Load data
data(mtcars)
# Basic scatter plot
qplot(x = mpg, y = wt, data = mtcars, geom = "point")
# Scatter plot with smoothed line
qplot(mpg, wt, data = mtcars, geom = c("point", "smooth"))


set.seed(1234)
wdata = data.frame(
        sex = factor(rep(c("F", "M"), each=200)),
        weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 3)


# Basic box plot from data frame
# Change fill color by sex
qplot(sex, weight, data = wdata, geom = "boxplot")


# Basic histogram
qplot(weight, data = wdata, geom = "histogram")


# Density plot with main titles and axis labels
qplot(weight, data = wdata, geom = "density", xlab = "Weight (kg)", ylab = "Density",main = "Density plot")

# Basic scatter plot
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()


# Change the point size, and shape
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(size = 1.5, shape = 18)

ggplot(mtcars, aes_string(x = "wt", y = "mpg")) + geom_point(size = 2, shape = 23)

# Use geometry function
ggplot(wdata, aes(x = weight)) + geom_density()

# OR use stat function
ggplot(wdata, aes(x = weight)) + stat_density()

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() + # to draw points
  geom_line() # to draw a line

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() + # to draw points
  geom_line(data = head(mtcars), color = "red")

# Log2 transformation in the aes()
ggplot(data = mtcars, aes(x = log2(wt), y = log2(mpg))) + geom_point() 


ggpoints <- function (data, xName, yName) {
  p <- ggplot(data = data, aes_string(xName, yName)) +
    geom_point(color = "red") +
    geom_smooth()
  return(p)
}



#Create a scatter plot using the helper function ggpoints():
ggpoints(mtcars, xName ="wt", yName = "mpg")


# Print the plot to a pdf file
pdf("myplot.pdf")
myplot <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
?ggplot
print(myplot)
dev.off()
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 4)
a <- ggplot(wdata, aes(x = weight))


library("dplyr")
mu <- wdata %>%
  group_by(sex) %>%
  summarise(grp.mean = mean(weight))
head(mu)
a+geom_area(weight) # for area plot
a+geom_density() # for density plot
a+geom_dotplot() # for dot plot
a+geom_freqpoly() # for frequency polygon
a+geom_histogram() # for histogram plot
a+stat_ecdf() # for empirical cumulative density function
x+stat_qq() # for quantile - quantile plot
x+geom_bar() # for bar plot


# Basic plot
# Change line and fill colors
a + geom_area(stat = "bin", color= "black", fill="#00AFBB")
#The following plots compares bar plots and area plots. The diamonds data set [in ggplot2 package] is used.
data("diamonds")
p <- ggplot(diamonds, aes(x = price, fill = cut))
# Bar plot
p + geom_bar(stat = "bin")
# Area plot
p + geom_area(stat = "bin")
# Basic density plot
a + geom_density()
# Change line color and fill color, add mean line
a + geom_density(color = "black", fill = "gray")+
  geom_vline(aes(xintercept=mean(weight)),
             color="#FC4E07", linetype="dashed", size=1)


# Change line colors by sex
a + geom_density(aes(color = sex))
# Change fill color by sex
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = sex), alpha=0.4)
# Add mean lines and color by sex
a + geom_density(aes(color = sex), alpha=0.4)+
  geom_vline(data = mu, aes(xintercept = grp.mean, color=sex),
             linetype="dashed")


# Basic histogram plot
a + geom_histogram()
# Change the number of bins
a + geom_histogram(bins = 50)
# Change line color and fill color, add mean line
a + geom_histogram(color = "black", fill = "gray")+
  geom_vline(aes(xintercept=mean(weight)),
             color="#FC4E07", linetype="dashed", size=1)


# Change line colors by sex
a + geom_histogram(aes(color = sex), fill = "white")
# Position adjustment: "identity" (overlaid)
a + geom_histogram(aes(color = sex), fill = "white", alpha = 0.6,
                   position="identity")
# Position adjustment: "dodge" (Interleaved)
# Add mean lines and color by sex
a + geom_histogram(aes(color = sex), fill = "white",
                   position="dodge") +
  geom_vline(data = mu, aes(xintercept = grp.mean, color=sex),
             linetype="dashed")


# Use classic theme and change legend position to "top"
# Change outline color manually
a + geom_histogram(aes(color = sex), fill = "white",
                   alpha = 0.4, position = "identity") +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))
# change fill and outline color manually
a + geom_histogram(aes(color = sex, fill = sex),
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))


# Combine Histogram and Density Plots
# Histogram with density plot
a + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=0.2, fill = "#FF6666")
# Color by groups
a + geom_histogram(aes(y=..density.., color = sex, fill = sex),
                   alpha=0.5, position="identity")+
  geom_density(aes(color = sex), size = 1)



### 7교시
setwd("c:/MyR")

# q1)
chosun <- read.csv("chosun.csv")
str(chosun)
class(chosun$즉위기간)

par(mfrow=c(1,2))
years <- chosun$즉위기간
stem(years) #줄기-잎그림
boxplot(years, 
        main="조선왕조 재위기간", 
        ylab="단위(년)") #상자그림
hist(years, 
     main="조선왕조 재위기간 히스토그램", 
     xlab ="재위기간(년)",
     col = "gray") #히스토그램

mean(years)
median(years)
summary(years)



# q2)
dat <- boxplot(years) #상자그림
dat


