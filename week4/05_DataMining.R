# 7월 27일 금요일 텍스트처리 기초

# stringr 패키지 
library(stringr)

# 문자열 결합
paste("I", "love", "R", sep = "-")
str_c("Letter", letters, sep = ":")
str_c("Letter", letters, sep = ":", collapse = "-")

# 문자열의 길이 계산
txts <- c("텍스트마이닝", "Text Mining", "자연어처리", "Natural Language Processing")
nchar(txts) # 기본함수
str_length(txts) # stringr 패키지 함수 

# 문자열 추출
txts <- c("텍스트마이닝", "Text Mining", "자연어처리", "Natural Language Processing")
substr(txts, start = 1, stop = 3) # 기본함수
str_sub(txts, start = 1, end = 3) # stringr 패키지 함수
str_sub(txts, start = -3, end = -1)

# 문자열 분할
txts <- c("https://terms.naver.com/entry.nhn?docld=1691554&cid=42171&categoryid=42183", 
          "http://www.bicdata.com/", "http://www.choicenews.co.kr/news/articleView.html?idxno=41371")
str_split(txts, pattern = "/")
str_split(txts, pattern ="[/&:.?=]") # 여러개 문자 지정
str_split(txts, pattern = "/", n = 2, simplify = T)

# 특정 문자가 있는지 검사
txts <- c("https://terms.naver.com/entry.nhn?docld=1691554&cid=42171&categoryid=42183", 
          "http://www.bicdata.com/", "http://www.choicenews.co.kr/news/articleView.html?idxno=41371")
str_detect(txts, pattern = "\\?")

# 특정 문자의 처음 위치와 마지막 위치
txts <- c("apple", "BlackBerry App World", "application", "pear", "pineapple")
str_locate(txts,"a")
str_locate(txts,"ap")
str_locate

# 특정 문자의 수 
txts <- c("https://terms.naver.com/entry.nhn?docld=1691554&cid=42171&categoryid=42183", 
          "http://www.bicdata.com/", "http://www.choicenews.co.kr/news/articleView.html?idxno=41371")
str_count(txts, pattern = "/")
str_count(txts, pattern = "[/&]")

# 문자열 반복
txts <- c("w", "/")
str_dup(txts, times = 3)
str_dup(txts, times = c(3,2))

# 특정 문자 변경
txts <- c("https://terms.naver.com/entry.nhn?docld=1691554&cid=42171&categoryid=42183", 
          "http://www.bicdata.com/", "http://www.choicenews.co.kr/news/articleView.html?idxno=41371")
str_replace(txts, pattern = "/", replacement = "-")
str_replace(txts, pattern = "/", replacement = "-")
str_replace_all(txts, pattern = "[/:.?=]", replacement = ",")

# 앞과 뒤 공백 제거
txts <- c("텍스트마이닝", "Text Mining", "자연어처리", "Natural Language Processing")
str_trim(txts, side = "both")
str_trim(txts, side = "left")
str_trim(txts, side = "right")


# KONLP 패키지
library(KoNLP)
library(stringr)

txts <- c("무엇보다 국민들께 대한민국의 국민이라는 자부심을 드리고자 한 1년이었습니다.",
          "변화를 두려워하고, 거부하고 앞으로 나가지 못하게 뒤에서 끌어당기는 힘이 여전히 강고합니다")

# 공백으로 텍스트 분리
strsplit(txts, split = " ") # 기본함수
str_split(txts, pattern = " ") # 패키지함수

# 명사 추출하기
extractNoun(txts)

# 품사까지 출력
SimplePos09(txts)
SimplePos22(txts)

# 문장에서 품사 확인하기
txts.Pos <- SimplePos09(txts)
str_match(txts.Pos[[2]], "([A-Z가-힣]+)/N") #명사

# 동사또는 형용사 확인
str_match(txts.Pos[[2]], "([A-Z가-힣]+)/P") 

# 명사, 용언 확인
str_match(txts.Pos[[2]], "([A-Z가-힣]+)/[NP]") 

## 예제 3.3
library(KoNLP)
library(stringr)

  # 사전실행
useSejongDic()
txts <- "유관순 열사와 안중근 의사는 독립투사임을 반드시 기억해야 합니다."
extractNoun(txts)

  # 단어 추가
Use.Word <- data.frame(word = c("유관순", "안중근", "열사"), verbose="ncn")
Use.Word

buildDictionary(ext_dic = c('sejong'), user_dic = Use.Word, replace_usr_dic = T)
extractNoun(txts)

detach("package:KoNLP", unload=TRUE)
detach("package:stringr", unload=TRUE)


## 예제 3.4
library(KoNLP)
library(stringr)

txts <- readLines("D://data03/dep.txt")  # 데이터 불러오기
txts
  
wd <- Map(extractNoun, txts)  # 명사 추출
wd

New.ls <- unique(wd)   # 중복 리스트 제거
New.ls

New.wd <- lapply(New.ls, unique)  # 리스트 성분 내에 중복데이터 제거
New.wd

# 데이터 제거
clr.wd <- rapply(New.wd, function(x) gsub("최고", "", x), how = "replace")
clr.wd <- rapply(New.wd, function(x) gsub("나", "", x), how = "replace")
clr.wd <- rapply(New.wd, function(x) gsub("[[:digit:]]", "", x), how = "replace")
clr.wd

# 길이가 3~5사이의 단어 필터링 함수 정의
filter1 <- function(x) {
  (nchar(x) <= 5 && nchar(x) >= 3)
}

# filter1() 함수를 적용하여 x벡터 단위 필터링
filter2 <- function(x){
  Filter(filter1, x)
}

# 줄 단어 대상 글자가 3글자 ~ 5글자 핕터링
lword <- sapply(clr.wd, filter2)
lword
