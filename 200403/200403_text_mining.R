# 힙합 가사 텍스트 마이닝

# 패키지 설치
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")

# 패키지 로드
library(KoNLP)
library(dplyr)

# 사전 설정하기
useNIADic()

# 데이터 준비
txt <- readLines("../datas/hiphop.txt") # 마지막줄에 엔터가 안눌려져있어서 불완전하게 마감되어있다는 경고메시지가 뜬다.

head(txt)

# 특수문자 제거
install.packages("stringr")
library(stringr)
txt <- str_replace_all(txt, "\\W", " ")
class(txt)
dim(txt)
View(txt)

# 가장 많이 사용된 단어 알아보기
# 명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")

# 가사에서 명사 추출
nouns <- extractNoun(txt) # 명사 추출 함수를 사용하면 리스트 타입으로 반환된다.
class(nouns)
dim(nouns)
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns)) # 리스트타입을 풀기 위해 unlist함수를 사용해야 table 타입으로 변환할 수 있다. 
class(wordcount)
dim(wordcount)
wordcount

# 자주 사용된 단어 빈도표 만들기
# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word <- rename(df_word, word=Var1, freq = Freq)
class(df_word)
dim(df_word)
# 두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
# 워드클라우딩 패키지 준비
library(wordcloud)
library(RColorBrewer)

# 단어 색상 목록 만들기
pal <- brewer.pal(8, "Dark2")

# 워드 클라우드 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,     # 최소 단어 빈도 
          max.words = 200,  # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1,     # 회전 단어 비율
          scale = c(4, 0.3),# 단어 크기 범위
          colors = pal)     # 색깔 목록

###############################################################################

# 국정원 트윗 텍스트 마이닝
# 국정원 계쩡 트윗 데이터
#   - 국정원 대선 개입 사실이 밝혀져 논란이 됐던 2013년 6월, 독립 언론 뉴스타파가 인터넷을 통해 공개한 것이다
#   - 국정원 계정으로 작성된 3,744개 트윗

# 데이터 로드
twitter <- read.csv("../datas/twitter.csv", header = T, stringsAsFactors = F, fileEncoding = "UTF-8")

# 변수명 수정
twitter <- rename(twitter,
                  no=번호,
                  id=계정이름,
                  date=작성일,
                  tw= 내용)
head(twitter)
class(twitter)
str(twitter)
dim(twitter)
names(twitter) # 컬럼명이 없을 경우 R은 자동으로 컬럼명을 X라고 붙여준다.

# 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter)

# 트윗에서 명사 추출
nouns1 <- extractNoun(twitter$tw)
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
nouns1 <- table(unlist(nouns1))
# 데이터 프레임으로 변환
df_twords <- as.data.frame(nouns1, stringsAsFactors = F)
# 변수명 수정
df_twords <- rename(df_twords,
                    word=Var1,
                    freq = Freq)

df_twords <- filter(df_twords, nchar(word) >= 3)

top_40 <- df_twords %>% 
  arrange(desc(freq)) %>% 
  head(40)

wordcloud(words = top_40$word,
          freq = top_40$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          scale = c(4,0.3),
          rot.per = .1,
          colors = pal)

# 단어 빈도 막대 그래프 만들기
library(ggplot2)

order <- arrange(top_40, freq)$word # 빈도 순서 변수 생성

ggplot(data=top_40, aes(x=word, y=freq))+
  ylim(0,2500)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit = order)+       # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3) # 빈도 표시