# 주제: 지하철역 주변 아파트 가격 알아보기
#------------------------------------------------
# 내용:
# 지하철 주변의 아파트 시세를 알아보고
# 지하철역에서 아파트까지의 거리에 따라
# 가격 분포가 어떻게 다른지를 비교
#------------------------------------------------
# 결과 시각화:
# 구글 지도를 활용하여 서울시 지도에
# 지하철역과 주변 아파트 실거래가 표시
#------------------------------------------------
# 주의 사항
# Google Map Platform 출시로 인해 ggmap패키지 변경됨

# 구글 API키가 반드시 필요.
googleAPIkey <-  "찾아서 넣으세요^_^"

#------------------------------------------------
# 활용 데이터

# 지하철역 주소 정보 : 서울 열린 데이터 광장
# URL : http://data.seoul.go.kr
# URL2 : http://data.seoul.go.kr/dataList/datasetView.do?infld=OA-12035&srvType=S&serviceKind=1

# 국토교통부 실거래가 공개 시스템
# URL : http://rtdown.molit.go.kr

# Google Maps Platform
# URL : https://cloud.google.com/maps-platfrom

# 사용하는 패키지
library(devtools)
library(ggmap)
library(dplyr)


# 2. 지하철역 데이터 가공하기
# 2-1. 원시 데이터 가져오기 csv파일 가져와서 station_data 변수에 할당
station_data <- read.csv("../datas/역별_주소_및_전화번호.csv")

# station_data의 "구주소"컬럼 속성 : Factor
# station_data 속성 확인
str(station_data)

# 2-2. 지하철역 좌표 정보 구하기

# as.character() 함수로 문자형으로 변환한 후 station_code에 할당
station_code <-  as.character(station_data$"구주소")

# google API key 등록
register_google(googleAPIkey)

# geocode() 함수로 station_code 값을 위도와 경도로 변환
station_code <- geocode(station_code)

# station_code 데이터 앞부분 확인
head(station_code)

# 인코딩이나 자료에 의해 데이터가 NA값으로 나올 경우
# 아래 처럼 인코딩을 변경하여 다시 좌표 정보를 구한다.
# 2-3. 지하철역 좌표 정보 구하기
# 문자형으로 변환하고 utf-8로 변환한 후 위도와 경도로 변환
station_code <- as.character(station_data$"구주소") %>% enc2utf8() %>% geocode()

# 기존 station_data에 위도/경도 정보를 추가.
# station_data와 station_code를 합친 후 station_code_final에 할당
station_code_final <- cbind(station_data, station_code)

# station_code_final의 앞부분 확인
head(station_code_final)

# 3. 아파트 실거래가 데이터 가공하기
# 3-1. 전용면적별 거래 가격

# csv파일을 가져와서 apart_data 변수에 할당
apart_data <- read.csv("../datas/아파트_실거래가.csv")

# 데이터 앞부분 확인
head(apart_data)

# ceiling() / floor()
# 전용면적의 값을 반올림하여 정수로 표현
apart_data$전용면적= round(apart_data$전용면적)

# 데이터 앞부분 확인
head(apart_data)

# count() : 지정한 집단별 행의 갯수
# count(데이터셋, 컬럼명)
# arrange() : 기본형(오름차순)/ arrange(desc())를 이용하면 내림차순 정렬 가능

# 전용 면적을 기준으로 빈도를 구한 후 빈도에 따라 내림차순 정렬
count(apart_data, 전용면적) %>% arrange(desc(n))

# 전용면적이 85인 데이터만 추출하여 apart_data_85에 할당
apart_data_85 <- subset(apart_data, 전용면적 == "85")

# 3-2. 아파트 단지별 평균 거래 금액

# 쉼표를 공백("")으로 대체하여 제거
apart_data_85$거래금액 <- gsub(",","", apart_data_85$거래금액)
# gsub : 지정한 컬럼에서 첫번째에 입력한 내용이있으면 두번째 내용으로 변환해주는 함수 (패턴, 변경사항, 데이터(혹은 칼럼))

# 결과 확인하기
head(apart_data_85)

# 거래금액을 정수형으로 변환하여 : as.integer(거래금액)
# 단지명별 평균을 구한 후 , mean(as.integer(거래금액)~ 단지명)
# apart_data_85_cost 변수에 할당.

# aggregate() : R내장함수, 그룹별로 묶어서 연산할 때 사용
# aggregate(집계할 내용, 데이터셋, 집계함수)
# 집계할 내용 : 연산컬럼명 ~ 기준컬럼명
# group_by를 안써도 처리를 해줄 수 있음 (일부 컬럼 추출후 퍼센트 계산할 때 등)

apart_data_85_cost <- aggregate(as.integer(거래금액) ~ 단지명,
                                apart_data_85,
                                mean)
head(apart_data_85_cost)

# as.integer(거래금액)dmf 거래금액으로 변경하여 저장
# rename(데이터셋, "변경후 이름"="변경전 이름")
apart_data_85_cost <- rename(apart_data_85_cost, "거래금액"="as.integer(거래금액)")

head(apart_data_85_cost)

# 단지명이 중복된 행을 제거하고, duplicated(apart_data_85$단지명)
# aprat_data_85에 저장

# duplicated() : 중복행 제거 함수 중복된 값(TRUE) / 처음나오는 값(FALSE) False에 해당하는 값들을 배열로 반환

apart_data_85 <- apart_data_85[!duplicated(apart_data_85$단지명),]

head(apart_data_85)

# "단지명"을 기준으로
# 중복 제거한 데이터셋(apart_data_85)에
# 평균 거래 금액 데이터 셋(apart_data_85_cost) 합치기

# 이 때 left_join() 함수를 사용.
# left_join(데이터 셋, 데이터 셋, 기준 칼럼)
# 합한 후, 동일한 컬럼 명이 존재할 경우,
# 컬럼명_x, 컬럼명_y 형태로 자동구분.

apart_data_85 <- left_join(apart_data_85, apart_data_85_cost ,by="단지명")

head(apart_data_85)

# 평균 거래금액("거래금액.y")을 이용하여 시각화 작업을 할 예정..
# 단지명, 시군구, 번지, 전용면적, 거래금액.y만 추출하고 저장
apart_data_85 <- apart_data_85 %>% select("단지명","시군구","번지","전용면적","거래금액.y")
head(apart_data_85)

# 거래금액.y 를 거래금액으로 변경 후 저장
apart_data_85 <- rename(apart_data_85, "거래금액"="거래금액.y")

# 3-3. 시군구와 번지를 하나로 합치기

# 테스트 코드
# 시군구와 번지 열을 합친 후, : paste(컬럼명, 컬럼명)
# apart_address에 저장
# 주의 사항 : paste() 함수를 이용하면 컬럼과 컬럼 사이에 공백 발생.
# 공백 없이 합할 경우 : paste0()함수를 사용.

apart_address <- paste(apart_data_85$"시군구",
                       apart_data_85$"번지") %>% data.frame()

# 결과 확인하기
head(apart_address) # 합했을 때의 컬럼명 설정을 안해주었기 때문에 컬럼명이 . 으로 나온다.

# .을 주소로 변경하여 저장
apart_address <- rename(apart_address,"주소"=".")
head(apart_address)

apart_address_code <- as.character(apart_address$"주소") %>% enc2utf8() %>% geocode()
View(apart_address_code)

# 데이터 세트를 합친 후,
# apart_data_85 :  단지명, 전용면적, 거래금액(평균거래금액) 추출
# apart_address : 주소 추출
# apart_address_code : 위도, 경도 추출
# 일부 열만 apart_code_final 에 저장
apart_code_final <- cbind(apart_data_85,
                          apart_address,
                          apart_address_code) %>% select("단지명","전용면적","거래금액","주소", lon, lat)
head(apart_code_final)

# 4. 구글 지도에 지하철역과 아파트 가격 표시하기
# 4-1. 마포구 지도 가져오기
# 마포구 지도 정보를 가져와 mapo_map에 저장
mapo_map <- get_googlemap("mapogu", maptype = "roadmap", zoom = 12)
# 구글 지도 호출
ggmap(mapo_map)

# 4-2. 지하철 역 위치 및 아파트 가격 정보 표시하기
library(ggplot2)

# 지하철역 위치 표시 및 역명 표시
ggmap(mapo_map) +
  geom_point(data = station_code_final,
             aes(x=lon, y=lat),
             colour = "magenta",
             size = 3) +
  geom_text(data = station_code_final,
            aes(label= 역명, vjust=-1))
# 홍대입구역 지도 정보를 가져와 hongdae_map 변수에 저장
hongdae_map <- get_googlemap("hongdae station", maptype="roadmap", zoom=15)
# 홍대 입구역 지도에 지하철 정보 및 아파트 정보 일괄 표시
ggmap(hongdae_map)+
  geom_point(data= station_code_final,
             aes(x=lon, y=lat),
             colour = "blue",
             size = 6)+
  geom_text(data = station_code_final,
            aes(label=역명, vjust=-1), colour="red")+
  geom_point(data = apart_code_final,
             aes(x=lon, y=lat))+
  geom_text(data = apart_code_final,
            aes(label=단지명, vjust=-1), colour="magenta")+
  geom_text(data = apart_code_final,
            aes(label = 거래금액, vjust=1), colour="orange")
