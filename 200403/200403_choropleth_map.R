# 단계 구분도 (choropleth Map)
# 지역별 통계치를 색깔의 차이로 표현한 지도
# 인구나 소득 같은 특성이 지역별로 얼마나 다른지 쉽게 이해할 수 있음

# 미국 주별 강력 범죄율 단계 구분도 만들기
# 패키지 준비하기
install.packages("ggiraphExtra")
library(ggiraphExtra)

# 내장 데이터셋 사용
str(USArrests)

head(USArrests)

library(tibble)

# 행 이름을 state 변수로 바꿔 데이터 프레임 생성
crime <- rownames_to_column(USArrests, var = "state")

# 지도 데이터와 동일하게 맞추기 위해 state의 값을 소문자로 수정
crime$state <- tolower(crime$state)

View(USArrests)

# tibble(티블)은 행이름을 가질 수 있지만 (예: 일반 데이터 프레임에서 변환 할 때) [연산자로 서브셋팅 할 때 제거 된다.
# NULL이 아닌 행 이름을 티블에 지정하려고 하면 경고가 발생한다.
# 일반적으로 행이름은 기본적으로 다른 모든 열과 의미가 다른 문자열 이므로, 행 이름을 사용하지 않는 것이 가장 좋다.
# 이러한 함수를 사용하면 데이터 프레임에 행 이름(has_rownames())가 있는지 감지하거나, 제거하거나(remove_rownames())
# 명시적 열 (rownames_to_column() 및 column_to_rownames()) 사이에서 앞뒤로 변환 할 수 있다.
# rowid_to_column()도 포함되어있다.
# 이것은 1부터 시작하여 순차적인 행 ID를 오름차순으로 하는 데이터 프레임의 시작부분에 열을 추가한다. 기존 행이름은 제거된다.

# 미국 주 지도 데이터 준비하기
library(maps)
states_map <- map_data("state")
str(states_map)
install.packages("mapproj")
library(mapproj)
# 단계 구분도 만들기
ggChoropleth(data = crime,
             aes(fill=Rape,
                 map_id = state),
             map = states_map,
             interactive=T)
# 옵션 : 지도에 표현할 데이터, 색깔로 표현할 변수, 지역 기준 변수, 지도 데이터, 인터렉티브

# 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
# 대한민국 시도별 인구 단계 구분도 만들기
# 패키지 준비하기
library(stringi)
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)

# 대한민국 시도별 인구 데이터 준비하기
str(changeCode(korpop1))

# 지도에 데이터를 뿌려주기 위한 선택 및 열이름 변경 작업.
library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(kormap1))

korpop2 <- rename(korpop2,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(kormap2))

korpop3 <- rename(korpop3,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(kormap3))
# 단계 구분도 만들기
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

ggChoropleth(data = korpop2,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap2,
             interactive = T)

ggChoropleth(data = korpop3,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap3,
             interactive = T)
# 위의 작업을 할때 korpop1 을 뿌리려면 kormap1을 사용해야한다. 2 3도 마찬가지로 같은 숫자로 맞추어 사용해야한다.

# 대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc))

ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# plotly 패키지로 인터랙티브 그래프 만들기
# 인터랙티브 그래프 만들기
# 패키지 준비하기
install.packages("plotly")
library(plotly)

# ggplot 으로 그래프 만들기
library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv))+geom_point()
ggplotly(p)
# 패키지 만든 사이트 : https://plotly.com/

p <- ggplot(data = diamonds, aes(x = cut, fill = clarity))+geom_bar(position = "dodge")
ggplotly(p)

# dygraphs 패키지로 인터랙티브 시계열 그래프 만들기
# 라이브러리 준비
install.packages("dygraphs")
library(dygraphs)

# 데이터 준비하기
economics <- ggplot2::economics
head(economics)

# 시간 순서 속성을 지니는 xts 데이터 타입으로 변경
library(xts)

eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
dygraph(eco) %>% dyRangeSelector()

# 여러 값 표현 하기
# 저축률
eco_a <- xts(economics$psavert, order.by = economics$date)

# 실업자수
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

# 합치기
eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert","unemploy")
head(eco2)

# 그래프 그리기
dygraph(eco2) %>% dyRangeSelector()
# 패키지 제작 사이트 주소 : http://dygraphs.com/

# 통계 분석 기법을 이용한 가설검정
# 통계적 가설 검정이란?
# 기술 통계와 추론 통계
# 기술 통계 : 데이터를 요약해 설명하는 통계 기법 ex) 사람들이 받는 월급을 집계해 전체 월급 평균 구하기
# 추론 통계 : 단순히 숫자를 요약하는 것을 넘어 어떤 값이 발생할 확률을 계산하는 통계 기법
# ex) 수집된 데이터에서 성별에 따라 월급에 차이가 있는 것으로 나타났을 때, 이런 차이가 우연히 발생할 확률을 계산

# 추론 통계 : 
# -이런 차이가 우연히 나타날 확률이 작다. -> 성별에 따른 월급차이가 통계적으로 유의하다. (statistically significant)고 결론
# -이런 차이가 우연히 나타날 확률이 크다. -> 성별에 따른 월급 차이가 통계적으로 유의하지 않다고 결론
# - 기술 통계 분석에서 집단 간 차이가 있는 것을로 나타났더라도 이는 우연에 의한 차이일 수 있음 
#   -> 데이터를 이용해 신뢰할 수 있는 결론을 내리려면 유의 확률을 계산하는 통계적 가설 검정절차를 거쳐야함

# 통계적 가설 검정
# 통계적 가설 검정(Statistical hypothesis test) - 유의 확률을 이용해 가설을 검정하는 방법
# 유의 확률 (Significance probability, p-value) :
#   - 실제로는 집단 간 차이가 없는데 우연히 차이가 있는 데이터가 추출될 확률
#   - 분석 결과 유의 확률이 크게 나타났다면
#     - 집단간 차이가 통계적으로 유의하지 않다고 해석
#     - 실제로 차이가 없더라도 우연에 의해 이정도의 차이가 관찰 될 가능성이 크다는 의미
#   - 분석 결과 유의 확률이 작게 나타났다면
#     - 집단 간 차이가 통계적으로 유의하다고 해석
#     - 실제로 차이가 없는데 우연히 이 정도의 차이가 관찰될 가능성이 작다, 우연이라고 보기 힘들다는 의미.

# compact 자동차와 suv 자동차의 도시 연비 t 검정
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff <- mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c("compact","suv"))
head(mpg_diff)
table(mpg_diff$class)

# t-test
t.test(data = mpg_diff, cty ~ class, var.equal = T)
# alternative hypothesis: true difference in means is not equal to 0 : 두 컬럼의 평균이 0이 아니면 유의하다

# 일반 휘발유와 고급 휘발유의 도시 연비 t검정
# 데이터 준비
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c("r","p")) # r: regular, p : premium
table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

# 상관분석 - 두 변수의 관계성 분석
# 상관 분석 (Correlation Analysis)
# 두 연속 변수가 서로 관련이 있는지 검정하는 통계 분석 기법
# 상관계수
#   - 두 변수가 얼마나 관련되어 있는지, 관련성의 정도를 나타내는 값
#   - 0~1 사이의 값을 지니고 1에 가까울수록 관련성이 크다는 의미
#   - 상관계수가 양수면 정비례, 음수면 반비례 관계.

# 실업자 수와 개인 소비 지출의 상관관계
# 데이터 준비
economics <- as.data.frame(ggplot2::economics)

# 상관분석
cor.test(economics$unemploy, economics$pce)

# 상관행렬 히트맵 만들기
# 상관행렬(Correlation Matrix)
#   - 여러 변수 간 상관계수를 행렬로 나타낸 표
#   - 어떤 변수 끼리 관련이 크고 적은지 파악할 수 있음.
# 여러항목들을 비교하기 위해서는 히트맵을 활용 하는 것이 유용하다.

# 데이터 준비
head(mtcars)

# 상관행렬 만들기 히트맵형식으로 보여주기 위해서는 반드시 matrix형태로 만들어 주어야한다.
car_cor <- cor(mtcars) # 상관 행렬 생성성
round(car_cor, 2)      # 소숫점 셋째 자리에서 반올림해서 출력력

# 상관행렬 히트맵 만들기
# - 히트맵 (heat map) : 값의 크기를 색으로 표현한 그래프
install.packages("corrplot")
library(corrplot)

corrplot(car_cor, method="number")
# 기본으로 입력하면 원모양으로 나오고 원 대신 상관계수를 표시하고 싶으면 위와 같이 입력하면된다.

# 다양한 파라미터 지정하기
col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(car_cor,
         method="color",
         col = col(200),
         type = "lower",
         order = "FPC",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)
# 데이터, 색깔로 표현, 색상 200개 선정, 왼쪽 아래 행렬만 표시, 유사한 상관계수끼리 군집화, 상관계수 색, 변수명 색
# 변수명 45도 기울임, 대각행렬 제외
install.packages()