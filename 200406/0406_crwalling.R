#################################################################
# [R] 파싱하여 데이터 가져오기
#################################################################
# 웹에서 문자열 가져오기
# revest 패키지의 read_html("url");

# html 파싱
# 특정 노드의 데이터 가져오기

# 문자열에서 html_nodes(태그 또는 클래스 또는 아이디)를 이용하면
# 매개변수에 해당하는 모든 데이터를 가져온다.
# 마지막 데이터까지 가져온 경우
# html_text() 메소드나 html_attr(속성명)을 이용.
install.packages("rvest")

library(rvest)

naver <-  read_html("http://www.naver.com")
naver

#################################################################
# html 파싱

# 특정 노드의 데이터 가져오기

# 테이블의 내용은 html_table() 메소드로
# 테이블의 데이터를 프레임으로 바로 변환해 준다.

# %>% 은 메소드의 연산결과를 가지고
# 다음 메소드를 호출 할 때 사용하는chain operation 연산자이다.

#################################################################
# 네이버 팟 캐스트 크롤링
#################################################################

# 크롤링을 할 때 가장 중요한 것은
# 원하는 부분을 구분할 수 있는 태그나 클래스 또는 아이디를 찾아내는 일이다.

# 고려해야 할 또 하나의 부분은
# 현재 페이지에 원하는 데이터가 있는지 아니면
# 링크만 존재하는지 확인하는 것.

# 1. 웹에서 데이터를 가져오기 위한 패키지 설치
# install.packages("rvest")

# 2.패키지 메모리 로드
library(rvest)

# 3. 문자열을 가지고 올 주소를 생성
url <- 'http://tv.naver.com/r/category/drama'

# 4. 문자열 다운로드
cast <- read_html(url)

# 5. 문자열 확인
cast

# 6. span안에 내용들이 출력
craw <- cast %>% html_nodes(".tit") %>% html_nodes('span')
craw

# 7. tit 클래스 안에 있는 span안에 내용 가져오기
craw <- cast %>% html_nodes(".tit") %>% html_nodes('span') %>% html_text()
craw

# 8. tooltip 태그 안의 내용 가져오기
craw <- cast %>% html_nodes("tooltip") %>% html_text()
craw

#################################################################
# 한겨레 신문에서 데이터를 검색한 후
# 검색 결과를 가지고 워드 클라우드 만들기

# 한겨례 신문사에서 지진으로 뉴스 검색하는 경우
# url
# http://search.hani.co.kr/Search?command=query&keyword=검색어입력부분&sort=d&period=all&media=news

# 기사 검색이나 SNS검색을 하는 경우
# 검색어를 가지고 검색하면 그 결과는 검색어를 가진 URL의 모임이다.
# 기사나 SNS글이 아니다.

# 검색결과에서 URL을 추출해서 그 URL의 기사내용을 다시 가져와야한다.
# dt 태그안에 있는 a태그의 href 속성의 값이 실제 기사의 링크이다.

# 실제 기사에서 클래스가 text안에 있는 내용이 기사 내용이다.

# 1. 기존의 변수를 모두 제거
rm(list=ls())

# 2. 필요한 패키지 설치
install.packages("stringr")      # 문자열 조작 패키지
install.packages("wordcloud")    # 시각화 작업할 때 필요한 패키지
install.packages("KoNLP")        # 명사 구분등의 작업에 필요한 사전 관련패키지
install.packages("dplyr")        # 데이터 프레임 조작하기 위한 전용패키지
install.packages("RColorBrewer") # 컬러 팔레트와관련된 패키지
install.packages("rvest")        # 웹페이지 크롤링 할 때 가장 기본이 되는 패키지

# 3. 필요한 패키지 로드
library(stringr)
library(wordcloud)
library(KoNLP)
library(dplyr)
library(RColorBrewer)
library(rvest)

# 4. 기사 검색 url을 생성
url <- 'http://search.hani.co.kr/Search?command=query&keyword=날씨&sort=d&period=all&media=news'

# 5. url에 해당하는 데이터 가져오기
k <- read_html(url, encoding="utf-8")
k

# 6. dt 태그 안에 있는 a 태그 들의 href 속성의 값을 가져오기
k <- k %>% html_nodes("dt") %>% html_nodes("a") %>% html_attr("href")
k

# 7. k에 저장된 모든 URL에 해당하는 데이터의 클래스가 text인 데이터를 읽어서 파일에 저장
# for(임시변수 in 컬렉션이름){
# }
for(addr in k){
  temp <- read_html(addr) %>% html_nodes(".text") %>% html_text()
  cat(temp, file = "temp.txt", append=TRUE)
}

# 8. 파일의 모든 내용 가져오기
txt <- readLines("temp.txt")
head(txt)

# 9. 명사만 추출
useNIADic()
nouns <- extractNoun(txt)
nouns

# 10. 빈도수 만들기 - 각 단어가 몇번씩 나왔는지
wordcount <- table(unlist(nouns))
wordcount

# 11. 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word
class(df_word)

# 12. 필터링
df_word <- filter(df_word, nchar(Var1) >= 2)

# 13. 워드클라우드 색상 판 생성
pal <- brewer.pal(8, "Dark2")

# 14. 시드 설정
set.seed(1234)

# 15. 워드 클라우드 생성
wordcloud(df_word$Var1, freq = df_word$Freq ,scale=c(4,0.3),rot.per=.1, min.freq = 2, max.words = 200, random.order = F, colors = pal)
library(wordcloud2)
In_out_colors = "function(word, weight) {return (weight > 10) ? '#B09BF0' : (weight > 5) ? '#F078B4' : (weight > 2) ? '#E7F076' : '#12F016' }"
wordcloud2(df_word,
           shape='rectangle',
           size=0.7,
           color = htmlwidgets::JS(In_out_colors),
           backgroundColor = "#D1E3E3")
#################################################################
# 코로나 19관련 기사로 크롤링 및 워드클라우딩 해보기

url2 <- 'http://search.hani.co.kr/Search?command=query&keyword=코로나19&sort=d&period=all&media=common'
k1 <- read_html(url2, encoding="utf-8")
k1 <- k1 %>% html_nodes("dt") %>% html_nodes("a") %>% html_attr("href")
for(addr1 in k1){
  temp1 <- read_html(addr1) %>% html_nodes(".text") %>% html_text()
  cat(temp1, file = "temp1.txt", append=TRUE)
}
txt1 <- readLines("temp1.txt")
nouns1 <- extractNoun(txt1)
wordcount1 <- table(unlist(nouns1))
df_word1 <- as.data.frame(wordcount1, stringsAsFactors = F)
df_word1 <- filter(df_word1, nchar(Var1) >= 2)
wordcloud2(df_word1,
           shape='rectangle',
           size=0.7,
           color = htmlwidgets::JS(In_out_colors),
           backgroundColor = "#D1E3E3")
# 동아일보 url 패턴 : http://news.donga.com/search?p=1&query=검색내용&check_news=1&more=1&sorting=1&search_date=1&v1=&v2=&range=1
url3 <- 'http://news.donga.com/search?p=1&query=코로나19&check_news=1&more=1&sorting=1&search_date=1&v1=&v2=&range=1'
k2 <- read_html(url3, encodign="utf-8")
k2 <- k2 %>% html_nodes(".txt") %>% html_nodes("a") %>% html_attr("href")
for(addr2 in k2){
  temp2 <- read_html(addr2) %>% html_nodes(".article_txt") %>% html_text()
  cat(temp2, file = "temp2.txt", append = TRUE)
}
txt2 <- readLines("temp2.txt")
nouns2 <- extractNoun(txt2)
nouns2 <- grep("[가-핳]", nouns2, value=T)
wordcount2 <- table(unlist(nouns2))
df_word2 <- as.data.frame(wordcount2, stringsAsFactors = F)
df_word2 <- filter(df_word2, nchar(Var1) >= 2)
wordcloud2(df_word2, shape='pentagon', size=0.7,color = htmlwidgets::JS(In_out_colors), backgroundColor = "black")


##########################################################
# R 키보드 이용한 데이터 입력

# c() 함수를 이용한 데이터 입력

# scan() 함수는 외부의 텍스트 파일을 불러올 때 외에도 키보드 입력에도 이용할 수 있는 함수 이며,
# 일반적으로 R Console에서 프롬프트가 [1] 과 같이 보여지나 scan()함수를 실행할 경우 '1:' 과 같은 형태로 출력된다.

# scan()함수이용
x <- scan()
# 문자열 입력의 경우
x <- scan(what=" ")

# edit()함수는 데이터 편집기 창을 직접 띄워 데이터를 직접 입력하는 방식으로
# 편집기는 셀의 형식을 띠고 있으며,
# 각 셀에 데이터를 입력한 후 수정할 경우는 
# 메뉴에서 <편집> - <데이터 편집기>를 선택하여 수정 할 수 있다.

# edit() 함수를 이용하여 데이터 입력기를 호출한 후 데이터를 입력한다.
age = data.frame()
age = edit(age)
age

# 정규표현식 관련 작업
library(KoNLP)
library(wordcloud)
library(stringr)

useSejongDic()

# 정규 표현식 예
text <- c("phone: 010-1234-5678", "home: 02-123-1234", "이름: 홍길동")
grep("[[:digit:]]", text, value=T) # 숫자가 있는 항목들만 보여주는 함수
gsub("[[:digit:]]", "x", text)     # 숫자를 x로 바꿔주는 함수

# 한글 문자열 분석에서 매우 중요한 정규표현식.
grep("[가-핳]", text, value=T)

# 영소문자를 포함하는 데이터 뽑기
grep("[[:lower:]]", text, value=T)

gsub("^[1-9][0-9]*$", " ", c("08","1","19 189","78"))

gsub("^[0-9]+(\\.[0-9]{1,2})?$", "zz", c("123","123.17","123.456","123.","12.79"))

# 블로거들이 추천하는 서울 명소 분석하기

# "seoul_go.txt" 파일을 사용하여 블로거들이 추천하는 서울 명소들을 워드 클라우드로 생성
# (서울명소추가 : 서울명소merge.txt)
# (제거단어 : 서울명소gsub.txt)

# setwd("c:\\r_temp")
# 필요 패키지를 설치하는데 이미 설치가 되어있어서 경고가 나오니 무시
library(KoNLP)
library(wordcloud)
library(stringr)

useSejongDic()

mergeUserDic(data.frame(readLines("../datas/서울명소merge.txt"),"ncn")) 
# ncn : 형태소 분석함수에는 위치값과 관련한 키들이 있다. 그중 하나로 ncn이라는 것이 있는데 이는 추후에 배울것이다.

txt <- readLines("../datas/seoul_go.txt")

place <- sapply(txt, extractNoun, USE.NAMES = F)

head(place, 10)
head(unlist(place), 30)

c <- unlist(place)
res <- str_replace_all(c, "[^[:alpha:]]", "")

txt <- readLines("../datas/서울명소gsub.txt")
txt

cnt_txt <- length(txt)
cnt_txt

for (i in 1:cnt_txt) {
  res <- gsub((txt[i]), "",res)
}

res2 <- Filter(function(x) {nchar(x) >= 2}, res)
nrow(res2)

write(res2, "../datas/seoul_go2.txt")
res3 <- read.table("../datas/seoul_go2.txt")
wordcount <- table(res3)
head(sort(wordcount, decreasing = T),30)

library(RColorBrewer)
palete <- brewer.pal(8,"Set2")
wordcloud(names(wordcount),freq = wordcount, scale=c(3,1), rot.per = 0.25, min.freq = 5, random.order = F, random.color = T, colors = palete)
legend(0.3,1,"블로거 추천 서울 명소 분석", cex=0.6, fill=NA, border=NA, bg="white",text.col="red",text.font=2, box.col="red")


