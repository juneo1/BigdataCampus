#농산물가격데이터 경로 지정
setwd("C:/Users/vic/Documents/R_day")

#데이터 로드 후 각 열의 이름을 영어로 변경
ds <- read.csv("grow.csv",  stringsAsFactors = FALSE)
names(ds) = c("date", "item", "item.type", "item.weight", "item.unit", "price", 
              "prod.grade", "distr.category", "city")

city <- c("서울", "대전", "대구", "부산", "인천")
count <- c(nrow(subset(ds, city == "서울")),  nrow(subset(ds, city == "대전")), nrow(subset(ds, city == "대구")), 
nrow(subset(ds, city == "부산")), nrow(subset(ds, city == "인천")))
data <- data.frame(city, count)
data

#### 예시다
dat = c()
city = c("서울", "대전", "대구", "부산", "인천")
for(i in 1:5) {
  this.city = city[1]
  this.ds = ds[ds$city == this.city, ]
  dat = rbind(dat,this.db)
}

d

p <- plot_ly(data, labels = ~city, values = ~count, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="pie-basic")
chart_link


#2-1.데이터중 서울시에 대한 데이터만을 추출한 뒤 품목단위가 "개"인 품목들을 출력
seouldata <- subset(ds, city == "서울" & item.unit == "개")
unique(seouldata[,2])

#2-2.위에서 출력한 것들 중 앞 5개에 대한 데이터만을 추출하여 ds.seoul 저장
seoul <- unique(seouldata[,2])

ds.seoul = ds[ds$city == "서울" & ds$item.unit == "개" & ((ds$item == seoul[1]) | (ds$item == seoul[2])|(ds$item == seoul[3])
              |(ds$item == seoul[4])|(ds$item == seoul[5])), ]


#2-3. ds.seoul을 활용하여 품목(item)에 대해 품목당 가격(price)의 평균을 구하고 이를 막대그래프로 표현,
# 위 파이차트와 마찬가지로  plotly로 구현한 뒤 이름 웹으로 업로드드

pineapple.mean = mean(ds.seoul[ds.seoul$item == "파인애플", "price"])
pumpkin.mean = mean(ds.seoul[ds.seoul$item == "호박", "price"])
lemon.mean = mean(ds.seoul[ds.seoul$item == "레몬", "price"])
mango.mean = mean(ds.seoul[ds.seoul$item == "망고", "price"])
melon.mean = mean(ds.seoul[ds.seoul$item == "멜론", "price"])

avg <- c(pineapple.mean, pumpkin.mean, lemon.mean, mango.mean, melon.mean)
item <- c(seoul[1], seoul[2], seoul[3], seoul[4], seoul[5])
avg <- data.frame(item,avg)
avg

p <- plot_ly(avg, x = ~item, y = ~avg, type = 'bar',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
  layout(title = "January 2013 Sales Report",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="bar-text")
chart_link
