#######################################################################################
# 서대문구에 치킨집이 많은 동은?
# 동별 치킨집 분포를 한눈에 파악할 수 있도록
# 트리 맵을 사용하여 시각화.

#######################################################################################
# 1. 업종별 데이터 다운로드 및 기초 가공

# LOCALDATA 웹 사이트
# http://www.localdata.kr
# 지방 자치 단체에서 인허가한 업종별 데이터를 제공
# 문화, 체육, 관광, 식품등 11가지 분야의 다양한 데이터 검색 가능.
#######################################################################################

#------------------------------------------------
# 치킨집 데이터 다운로드.
#------------------------------------------------
# 1. http://www.localdata.kr 접속
# 2. 메뉴 중 "데이터 받기" 선택
# 3. 오른쪽으로 세번째 " 지역 다운로드" 선택
# 4. "서울 특별시"의 다운로드 버튼 클릭.
# 5. "6110000.zip" 파일 압축 풀기

#------------------------------------------------
# 불필요한 데이터 제거.
#------------------------------------------------
# 1. "6110000_서울특별시_07_24_04_P_일반음식점.xlsx" 파일
# 2. 서대문구 치킨집 데이터 확인.
# 3. 주소 : 서대문구, 업태 : 호프/통닭인 데이터만 남기기.


#######################################################################################
# 2. 데이터 가공 및 트리 맵 표현.

# 동별 분포를 확인하기 위해 소재지 전체 주소열에서
# 동별 업소 개수를 확인 한 후,
# Rstudio에서 추가로 가공하여 트리 맵을 이용한 시각화.
#######################################################################################

#===========================================================
# 소재지 전체 주소 가공하기
#===========================================================
# 우선 전체 주소를 가공하여 동을 파악.

#------------------------------------------------
# 1. readx1 패키지를 로드하여 .xlsx 파일 읽어오기
library(readxl)

ck <- read_xlsx("../datas/치킨집_가공.xlsx")

#------------------------------------------------
# 2. '소재지전체주소' 열에서 'xxx동'만 남기고
# 이후 상세 주소 삭제.....

# substr(데이터, 시작위치, 끝위치)
# substr() 함수를 사용하여
# '서울특별시 서대문구' 다음 글자인 12번째 글자부터
# 'XXX동' 까지 포함되도록 15번째 글자까지를 잘라내기
# 예) 남가좌동

# '소재지전체주소' 열에서 12번째 글자부터 16번째 앞글자까지를 추출

addr <- substr(ck$소재지전체주소, 12, 16)
head(addr)

#------------------------------------------------
# 3. 동 이름이 3글자와 4글자인 경우가 있어 
# 3글자인 경우 "창천동 5"와 같이 숫자가 포함.

# gsub("어떤 글자를", "무엇으로", "어디에 있는")
# gsub()함수를 사용하여 공백과 숫자를 제거

# 숫자 제거
addr_num <- gsub("[0-9]", "" , addr)
addr_num

# 공백 제거
addr_trim <- gsub(" ", "", addr_num)
head(addr_trim)

#===========================================================
# 동별 업소 개수 확인.
#===========================================================
# 동별 개수를 확인하여 트리맵으로 표현할 준비작업.

# table() : 변수 개수를 확인하기 위한 함수
# 사용형식 : table(데이터셋1, 데이터셋2)
#            데이터의 옵션 값이 1개면 도수분포도를 만들고,
#            2개면 교차표를 생성.

# 도수분포표 : 항목별 개수를 나타낸 것(동별 개수를 파악)
# 교차표 : 2가지 변수에서 항목간 빈도를 파악할 수 있도록 작성한 표

#------------------------------------------------
# 1. table() 함수를 사용하여 도수분포표를 생성.
# 2. 이를 dplyr 패키지의 %>%연산자로 연결
# 3.  data.frame() 함수를 이용하여 데이터 프레임으로 변환.

library(dplyr)

# table() 함수를 이용해서 숫자 세기.
# 변수가 한개일 때 도수분포표를 만들어줌

addr_count <- addr_trim %>% table() %>% data.frame()
head(addr_count)

# 트리 맵으로 표현.
#===========================================================
# treemap 패키지에 있는
# treemap() 함수를 이용하여 시각화.

# 사용 형식 : 
# treemap(데이터 셋, index=구분 열,
#           vSize= 분포열,
#           vColor = 색상, title = 제목)

# treemap(데이터 셋, index=인덱스 표시 열 제목,
#           vSize= 크기를 이용할 열 제목,
#           vColor = 컬러, title = 제목)

#------------------------------------------------
# 1.treemap 패키지 설치 및 로드

install.packages("treemap")
library(treemap)

#------------------------------------------------
# 2. 동이름 열(.)과 치킨집 개수열(Freq)로 트리 맵 시각화

treemap(addr_count,
        index=".",
        vSize = "Freq",
        title = "서대문구 동별 치킨집 분표")

#######################################################################################
# 서울시 주요 구의 의원 현황을 한꺼번에 보여주기

# 건강보험 심사평가원 홈페이지에서 다운로드 받은
# "2013년_서울_주요구별_병원현황.csv" 파일을 사용해서
# 서울시 주요 구의 의원 현황을 아래 예시와 같이 각 구별로 나누어서 출력

data1 <- read.csv("../datas/2013년_서울_주요구별_병원현황.csv", header=T)

data1

# 새로운 출력 디바이스 생성
dev.new()

# 필요한 데이터를 각각의 변수에 할당.
v1 <- data1[1:9, 2]*0.1 # 강남구 값
v2 <- data1[1:9, 3]*0.1 # 강동구 값
v3 <- data1[1:9, 4]*0.1 # 강서구 값
v4 <- data1[1:9, 5]*0.1 # 관악구 값
v5 <- data1[1:9, 6]*0.1 # 구로구 값
v6 <- data1[1:9, 7]*0.1 # 도봉구 값
v7 <- data1[1:9, 8]*0.1 # 동대문구 값
v8 <- data1[1:9, 9]*0.1 # 동작구 값
v9 <- data1[1:9, 10]*0.1 # 마포구 값
v10 <- data1[1:9, 11]*0.1 # 서대문구 값

View(data1)

# 2행 5열로 그래프 배치하기
# 실제 그래프는 표현 안됨.
par(mfrow=c(2,5)) # dev에서 그래프를 여러개로 볼 수 있게 분할 시켜준다.

name <- data1$표시과목
name

# 강남구 그래프 그리기
gangnam <- barplot(as.matrix(v1),
                   main="강남구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

gangbuk <- barplot(as.matrix(v2),
                   main="강북구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

############################################################################
savePlot("../datas/hospital2.png",type="png")

gangseo <- barplot(as.matrix(v3),
                   main="강서구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

gwanak <- barplot(as.matrix(v4),
                   main="관악구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

guro <- barplot(as.matrix(v5),
                   main="구로구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

dobong <- barplot(as.matrix(v6),
                   main="도봉구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

dongdaemun <- barplot(as.matrix(v7),
                   main="동대문구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

dongjak <- barplot(as.matrix(v8),
                   main="동작구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

mapo <- barplot(as.matrix(v9),
                   main="마포구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션

seodaemun <- barplot(as.matrix(v10),
                   main="서대문구 병원현황",
                   beside=T,
                   axes=F,
                   ylab="병원수(단위:10개)",
                   xlab="",
                   cex.names=0.85,
                   las=2,
                   ylim=c(0,40),
                   col=rainbow(8),
                   border="white",
                   names.arg=name)
axis(2,ylim=seq(0,35, 10)) # 축 지정

abline(h=seq(0,35,5), lty=2) # 베이스 라인 지정
# lty :  linetype 선의 모양은 어떻게 뿌려주는지 지정해주는 옵션