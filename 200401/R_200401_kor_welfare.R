install.packages("foreign")
library(foreign)  # SPSS 파일 로드
library(dplyr)  # 전처리
library(ggplot2)  # 시각화
library(readxl) # 엑셀 파일 불러오기

# 데이터 준비하기
# 데이터 불러오기
raw_welfare <- read.spss(file="../datas/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

# 복사본 만들기
welfare <- raw_welfare

# 데이터 검토
head(welfare)
tail(welfare)
dim(welfare)
str(welfare)
summary(welfare)
# 대규모 데이터는 변수가 많고 변수명이 코드로 되어있어서, 데이터 구조를 한눈에 파악하기 어렵다. 변수명을 쉬운 단어로 바꾼 후 분석에 사용할 변수들을 각각 파악해야한다.

# 변수명 바꾸기
welfare <- rename(welfare,
                  sex=h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7) # 지역 코드

# 성별에 따른 월급 차이
# 분석 절차
# 1. 변수 검토 및 전처리
# 성별, 월급
# 2. 변수 간 관계 분석
# 그래프 만들기
# 1. 변수 검토 하기 
class(welfare$sex)
table(welfare$sex)

# 2. 전처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) # 이상치 결측 확인
table(is.na(welfare$sex)) # 결측지 확인
welfare$sex <- ifelse(welfare$sex ==1, "male","female") # 성별 항목 이름 부여
table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
# 1. 변수 검토 하기
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)

# 2. 전처리
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income) # 이상치 결측 처리
table(is.na(welfare$income)) # 결측치 확인

# 2-1. 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income

# 2-2. 그래프 만들기
ggplot(data=sex_income, aes(x=sex, y=mean_income))+geom_col()

# 3. 나이와 월급의 관계 - 몇 살때 월급을 가장 많이 받을까?
# 분석 절차
# 1. 변수 검토 하기
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
# 2. 전처리
table(is.na(welfare$birth)) # 결측치 확인
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) # 이상치 결측 처리
table(is.na(welfare$birth))
# 3. 파생 변수 만들기 - 나이
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)

# 나이에 따른 월급 평균표 만들기
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income= mean(income))
head(age_income)
# 그래프 만들기
ggplot(data=age_income, aes(x=age, y=mean_income))+geom_line()

# 4. 연령대에 따른 월급 차이 - 어떤 연령대의 월급이 가장 많을까?
# 파생 변수 만들기 - 연령대
welfare <- welfare %>% 
  mutate(ageg=ifelse(age<30, "young",
                     ifelse(age <=59, "middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)
# 연령대에 따른 월급 차이 분석하기
# 1. 연령대별 월급 평균표 만들기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
ageg_income
ggplot(data=ageg_income,aes(x=ageg, y=mean_income))+geom_col()
# 막대 정렬 초년, 중년, 노년 나이순
ggplot(data=ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits = c("young","middle","old"))
# cf)  scale_축_discrete(limits = 내용) :  축의 순서를 강제로 고정시켜줌
# reorder와의 차이는 reorder는 데이터 크기에 따라 재배치 하는 것이다.

# 연령대 및 성별 월급 차이 - 성별 월급차이는 연령대 별로 다를까?
# 분석절차
# 1. 변수 검토 및 전처리 (연령대 , 성별, 월급 )-> 위에서 이미 전처리가 완료됨!
# 2. 변수간 관계 분석(연령대 및 성별 월급 평균표 만들기, 그래프 만들기)
# 연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+geom_col()+scale_x_discrete(limits=c("young","middle","old"))
# cf) 스택 차트에서는 fill을 사용하면 파이썬에서 보던 hue의 역할을 대체 할 수 있다.
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+geom_col(position="dodge")+
  scale_x_discrete(limits=c("young","middle","old"))
# cf) geom_col내부에 position옵션에 dodge를 입력하면 겹쳐지지 않고 피해서 그래프를 그려준다!
  
# 나이 및 성별 월급 차이 분석하기
# 성별 연령별 월급 평균표 만들기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income=mean(income))
sex_age
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex))+geom_line()
# cf)라인 차트의 경우 col을 입력해야한다.

# 직업별 월급 차이 구하기 - 어떤 직업이 더 많은 돈을 버는가
class(welfare$code_job)
table(welfare$code_job)

# 2. 전처리 직업 분류 코드 목록 불러오기
list_job <- read_excel("../datas/Koweps_Codebook.xlsx", col_names=T, sheet=2) # sheet 2번째에 있는 내용을 뽑음
head(list_job)
dim(list_job)

# welfare에 직업명 결합
welfare <- left_join(welfare, list_job, id="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

# 직업별 월급 차이 분석하기
# 1. 직업별 월급 평균표 만들기
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))
head(job_income)

# 2. 상위 10개 추출
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income))+geom_col()+coord_flip()

# 4. 하위 10개 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data=bottom10, aes(x=reorder(job, -mean_income),y=mean_income))+geom_col()+coord_flip()+ylim(0,850)
# 상위 10개와의 비교를 쉽게하기 위해 상한선을 만들어서 값을 한정지어 주어야 비교하기 편하다.

# 성별 직업 빈도 - 성별로 어떤 직업이 가장 많을 까?
# 1. 성별 직업 빈도표 만들기
# 남성 직업 빈도 상위 10개 추출
job_male <- welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
# n=n() 해당 직업을 갖고 있는 사람들의 수를 합하는 과정.
job_male
# 여성 직업 빈도 상위 10개 추출
job_female <- welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

# 2. 그래프 만들기
# 남성 직업 빈도 상위 10개 직업
ggplot(data= job_male, aes(x=reorder(job, n), y=n))+ geom_col() +coord_flip()
# 여성 직업 빈도 상위 10개 직업
ggplot(data= job_female, aes(x=reorder(job, n), y=n))+ geom_col() +coord_flip()

# 종교 유무에 따른 이혼율 - 종교가 있는 사람들이 이혼을 덜 할 까?
# 변수 검토하기
class(welfare$religion)
table(welfare$religion)

# 전처리 종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

# 혼인 상태 변수검토 및 전처리 하기
class(welfare$marriage)
table(welfare$marriage)

# 이혼 여부 변수 만들기 
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# 종교 유무에 따른 이혼율 표만들기
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
religion_marriage

# 위의 작업을 count()를 활용하여 표현
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)* 100, 1))
religion_marriage

# 이혼율 표만 들기 
# 이혼 추출
divorce <-religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)
divorce
# 그래프 만들기
ggplot(data=divorce, aes(x=religion , y=pct ))+geom_col()

# 연령대 및 종교 유무에 따른 이혼율 분석하기
# 1. 연령대별 이혼율 표 만들기
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n= n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

# count()함수로 활용하는 방법
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct=round(n/sum(n)*100, 1))
ageg_marriage

# 2. 연령대별 이혼율 그래프 만들기
# 초년 제외, 이혼 추출
ageg_divorce <- ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)
ageg_divorce
# 그래프 만들기
ggplot(data= ageg_divorce, aes(x=ageg, y=pct))+geom_col()

# 3. 연령대 및 종교 유무에 따른 이혼율 표 만들기
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
ageg_religion_marriage

# count 활용
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))
ageg_religion_marriage

# 연령대 및 종교 유무별 이혼율 표 만들기
df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)
df_divorce

ggplot(data= df_divorce, aes(x=ageg, y=pct, fill=religion))+geom_col(position = "dodge")

#############################################################################################
# 지역별 연령대 비율 - 노년층이 많은 지역은 어디일까?
# 1. 변수 검토하기
class(welfare$code_region)
table(welfare$code_region)

# 2. 전처리
# 지역 코드 목록 만들기
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

# welfare에 지역명 변수 추가
welfare <- left_join(welfare, list_region, id="code_region")
# 확인
welfare %>% 
  select(code_region, region) %>% 
  head
# 1. 지역별 연령대 비율표 만들기
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n= n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,2))
head(region_ageg)
# count활용
region_ageg <- welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100, 2))
# 2. 그래프 만들기
ggplot(data= region_ageg, aes(x=region, y= pct, fill=ageg))+geom_col()+coord_flip()

# 3. 막대 정렬하기 : 노년층 비율 높은순
# 노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

list_order_old

# 지역명 순서 변수 만들기
order <-  list_order_old$region
order

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+geom_col()+coord_flip()+scale_x_discrete(limits=order)
