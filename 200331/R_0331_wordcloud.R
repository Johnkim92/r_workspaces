# 작업 순서
# 1. 데이터 파일 읽기 : 6_101_DT_1B26001_A01_M.csv (국내 인구 이동통계)
#   다운로드 사이트 : http://kostat.go.kr
#   data <- read.csv(file.choose(),header=T)
# 2. '전국' 지역이 아닌 데이터만 추출
# data[data$행정구역.시군구.별 != "전국",]
# 3. 행정구역 중 '구' 단위에 해당하는 행 번호 추출
#   x <- grep("구$", data2$행정구역.시군구.별)
# 4. '구' 지역 데이터 제외
#   data3 <- data2[-c(x)]
# 5. 순이동 인구수가 0보다 큰 지역 추출
#   data4 <- data3[data3$순이동.명>0]
# 6. 단어(행정 구역) 할당
#   data4$행정구역.시군구.별
# 7. 행정구역별 빈도: data$순이동.명
# 8. 워드클라우드 출력 : wordcloud()

data <- read.csv(file.choose(),header = T)
head(data)

# 데이터 정제: 불필요 지역 제외
# '전국' 지역 제외
data2 <- data[data$행정구역.시군구.별 != "전국", ]
head(data2)

# '구' 단위 지역 통계 제외
x <- grep("구$", data2$행정구역.시군구.별)
data3 <- data2[-c(x),]
head(data3)

# 전입자 수가 많은 지역
data4 <- data3[data3$순이동.명.>0,]
head(data4)

word <- data4$행정구역.시군구.별
View(word)

frequency <- data4$순이동.명.
View(frequency)

pal2 = brewer.pal(8,"Dark2")
wordcloud(word, frequency, colors=pal2)