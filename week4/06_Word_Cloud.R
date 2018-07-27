# 7월 27일 금요일 워드 클라우드


## 예제 3.5

library(wordcloud)
library(KoNLP)
library(stringr)

useSejongDic()

Ins.WD <- data.frame(word = readLines("D://data03/InsWD.txt")) # 추가할 단어
Del.WD <- readLines("D://data03/DelWD.txt") # 삭제할 단어

# 사전에 단어 추가
buildDictionary(ext_dic = 'sejong', user_dic = Ins.WD, replace_usr_dic = T)

# QnA 데이터 불러오기
txt.Data <- readLines("D://data03/QnA.txt")

# 명사 추출
wd <- Map(extractNoun, txt.Data)
head(wd)

# 중복 리스트 제거
New.IS <- unique(wd)
head(New.IS)

# 리스트 성분 내에 중복 데이터 제거
New.wd <- lapply(New.IS, unique)
head(New.wd)

# 데이터 제거
clr.wd <- New.wd
m <- length(Del.WD)
for(i in 1:m) {
  clr.wd <- rapply(clr.wd, function(x) gsub(Del.WD[i], "", x), how = "replace")
}
head(clr.wd)

# 길이가 2~10사이의 단어 필터링 함수 정의
filter1 <- function(x) {
  (nchar(x) <= 10 && nchar(x) >= 2)
}

# filter1() 함수를 적용하여 x벡터 단위 필터링
filter2 <- function(x){
  Filter(filter1, x)
}

# 줄 단어 대상 글자가 3글자 ~ 5글자 핕터링
lword <- sapply(clr.wd, filter2)
head(lword)

# 리스트를 벡터로 변환
TotalWD <- unlist(lword)

# 전체 단어별 빈도 내보내기
WDCount <- sort(table(TotalWD), decreasing = T)
head(WDCount)
head(TotalWD, n=20)

# 워드클라우드 작성
WDName <- names(WDCount)
windows()
wordcloud(WDName, WDCount, min.freq = 5, scale = c(5,.5), rot.per = .5, random.order = F, 
          random.color = T, colors = rainbow(sum(WDCount)))
