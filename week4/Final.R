setwd("D:/data")

# 1.지수평활법과 ARIMA

library(forecast)

house.data <- read.csv("house.csv")
View(house.data)
house <- ts(house.data, start = c(1997,1), frequency = 4)

# 단순 지수평활법 
ha <- HoltWinters(house, beta = F, gamma = F)
ha <- HoltWinters(house)
plot(ha)
ha_simple <- HoltWinters(house)
plot(ha_simple)
  
# 예측값
ha$fitted
forecast(ha, h=4)


#2-1 
library(ggplot2)

bank <- read.table("D://data/Bank.txt")
bank <- read.csv("Bank.csv")
bank

#데이터와 변수 할당
xp <- ggplot(data <- bank, aes(x=InvAmount, y=..density..), xlab("투자금액(단위: 만원"))

#형태 histogram 지정
xp + geom_histogram(bins=10, binwidth = 10,  fill="darkblue") + geom_density(alpha=0.3, fill="yellow")

#2-2

View(bank)
bank
bank.20 <- subset(bank, "Age" = "20대")
View(bank.20)
bank.30 <- subset(bank, "Age" = "30대")
bank.30
bank.40 <- subset(bank, "Age" = "40대")
bank.50 <- subset(bank, "Age" = "50대")
bank.60 <- subset(bank, "Age" = "60대")

scatp <- ggplot(data = bank, aes(x=Income, y=InvAmount)) + geom_jitter
scatp + geom_point(alpha = 0.75, colour = "red", shape = 15, size = 2.5) + geom_smooth(alpha=0.3, colour="blue", fill="blue" , linetype = 3 )


#2-3

bank.man <- subset(bank, "Gender" = "남성")
bank.female <- subset(bank, "Gender" = "여성")

barp <- ggplot(data = bank.man, aes(x=Online, y=InvAmount), ggtitle("남자"), xlab="투자금액(단위:만원)")
barp + geom_boxplot(alpha=0.25, colour="navy", fill = "magenta") + coord_flip()

barp1 <- ggplot(data = bank.female, aes(x=Online, y=InvAmount))
barp1 + geom_boxplot(alpha=0.25, colour="navy", fill = "magenta") + coord_flip()

install.packages("gridExtra")
library(gridExtra)


grid.arrange(barp, barp1, ncol=2)



#3. 

install.packages("ggmap")
library(ggmap)



names <- c("우리집", "단국대학교 죽전캠퍼스")
addr <- c("경기도 성남시 분당구 내정로 185", "경기도 용인시 수지구 죽전로 152")

gc <- geocode(enc2utf8(addr))
gc

df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
df

cen <- c(mean(df$lon), mean(df$lat))
cen

map <- get_googlemap(center=cen, maptype = "roadmap", zoom = 14,  markers = gc)
ggmap(map)
