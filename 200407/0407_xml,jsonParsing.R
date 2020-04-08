# 금일 날씨 데이터
# http://www.weather.go.kr/weather/forecast/mid-term-xml.jsp?stnld=109

#############################################################################################################
# XML 파싱
#############################################################################################################
# html 파싱과 동일 
# XML은 엄격한 HTML이고 태그의 해석을 브라우저가 하지 않고
# 클라이언트가 직접한다는 점이 HTML과 다른점
# 모든 HTML 파싱 방법은 XML에 적용 가능.
# XML파싱 방법으로는 파싱하지 못하는 HTML이 있을 수 도 있다.

# 1. 태그의 내용 가져오기
# 문자열을 가지고 올 주소를 생성
url <- 'http://www.weather.go.kr/weather/forecast/mid-term-xml.jsp?stnld=109'

# 문자열 다운로드
library(rvest)
weather <- read_html(url)
weather

tmn <- weather %>% html_nodes("tmn") %>%  html_text()
tmn


#############################################################################################################

# JSON 샘플 데이터
# https://api.github.com/users/hadley/repos

# JSON 파싱

# jsonlite 패키지와 httr패키지를 이용.
# fromJSON() 함수에 URL을 대입하면 data.frame 으로 리턴.

# 즉, https://api.github.com/users/hadley/repos 데이터를
# data.frame 으로 변환

# 1. 필요한 패키지 설치
install.packages("jsonlite")
install.packages("httr")

# 2. 필요한 패키지 로드
rm(list=ls())
library(jsonlite)
library(httr)
df <- fromJSON("https://api.github.com/users/hadley/repos")
df
