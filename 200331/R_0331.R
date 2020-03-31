x <- c(9,15,20,6)
label <- c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
pie(x, labels=label, main="부서별 영업 실적")

View(x)

pie(x, init.angle = 90,labels = label, main = "부서별 영업 실적")

# 색과 라벨 수정
pct <-  round(x/sum(x)*100)
label <- paste(label, pct)
label <- paste(label, "%", sep="")
pie(x, labels=label, init.angle = 90, col=rainbow(length(x)),main="부서별 영업 실적")

# 3D 파이 차트
pie3D(x, labels=label, explode=0.1, labelcex=0.8, main="부서별 영업 실적")
# explode 차트데이터 간의 간격 비율 labelcex 라벨과 차트의 크기비율

# 기본 바차트 출력
height <- c(9,15,20,6)
name <- c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
barplot(height, names.arg=name, main="부서별 영업 실적")
# 막대의 색 지정
barplot(height, names.arg=name, main="부서별 영업 실적", col=rainbow(length(height)))
# 라벨 수정
barplot(height, names.arg=name, main="부서별 영업 실적", col=rainbow(length(height)), xlab="부서", ylab="영업 실적(억 원)", ylim=c(0,25))
# 데이터 라벨 출력
bp <- barplot(height, names.arg=name, main="부서별 영업 실적", col=rainbow(length(height)), xlab="부서", ylab="영업 실적(억 원)", ylim=c(0,25))
# 데이터 라벨을 출력하기 위해서는 차트를 변수에 저장을 해놓아야한다.
text(x=bp, y=height, labels=round(height, 0), pos=3)
# 데이터 라벨을 뽑아내기 위해서는 값을 지정해주어야한다.
# pos는 포지션 위치의 약자이다. 1일 경우 내부에 들어가고 3일경우 기준선의 위에 숫자가 생긴다.

# 바차트의 수평 회전(가로 막대)
barplot(height, names.arg=name, main="부서별 영업 실적", col=rainbow(length(height)), xlab="영업 실적(억 원)", ylab="부서",horiz=TRUE, width=50)
# horiz 옵션으로 가로를 만들어줄 수 있다. width는 그래프의 폭을 결정짓는다.

# 스택형 바차트(두가지 값을 쌓아서 올리는 그래프)
height1 <- c(4,18,5,8)
height2 <- c(9,15,20,6)
height3 <- c(3,10,15,8)
height <- rbind(height1,height2,height3)
View(height)
name <- c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
legend_lbl <- c("2014년","2015년","2016년")
barplot(height, names.arg=name,main="부서별 영업 실적", xlab="부서", ylab="영업 실적(억 원)", col=c("blue","red","green"), legend.text=legend_lbl, ylim=c(0,50))
# beside를 사용하면 그룹형 바차트로 표현 할 수 있다.
barplot(height, names.arg=name,main="부서별 영업 실적", xlab="부서", ylab="영업 실적(억 원)", col=c("blue","red","green"), legend.text=legend_lbl, ylim=c(0,50),beside=TRUE, args.legend=list(x='top'))

# 일반적인 X-Y 플로팅
View(women) # 내장데이터는 뷰함수를 쓰면 자동으로 볼 수 있다.
weight <- women$weight
plot(weight)
height <- women$height
plot(height, weight, xlab="키", ylab="몸무게")
# 플로팅 문자의 출력
plot(height, weight, xlab="키", ylab="몸무게",pch=22, col="blue",bg="yellow",cex=1.5)
# pch=> 마름모꼴로 포인트를 표현 bg: 배경색 cex: 크기 col: 태두리선의 색

# 지진의 강도에 대한 히스토그램
head(quakes)

mag <- quakes$mag
mag
hist(mag, main="지진 발생 강도의 분포", xlab="지진 강도", ylab="발생 건수")
# 계급 구간과 색
colors <- c("red","orange","yellow","green","blue","navy","violet")
hist(mag, main="지진 발생 강도의 분포", xlab="지진 강도", ylab="발생 건수", col=colors, breaks=seq(4, 6.5, by=0.5))
# 확률 밀도
hist(mag, main="지진 발생 강도의 분포", xlab="지진 강도", ylab="발생 건수", col=colors, breaks=seq(4, 6.5, by=0.5),freq=FALSE)
lines(density(mag))
# 확률 밀도를 구해주는 함수는 density이다.
# breaks는 히스토그램의 구간을 만들어주는 옵션이다. 
# lines 함수는 그래프에 선을 만들어준다.

# 박스 플롯
mag < quakes$mag
min(mag)
max(mag)
median(mag)
mean(mag)
quantile(mag, c(0.25,0.5,0.75)) # 사분위 값을 구해주는 함수
boxplot(mag, main="지진 발생 강도의 분포", xlab="지진", ylab="발생 건수", col="red")

# 지역별 순이동에 따른 워드 클라우드
#install.packages("wordcloud")
#library(wordcloud)
word <- c("인천광역시","강화군","옹진군")
frequency <- c(651,85,61)
wordcloud(word, frequency, colors="blue")
# 최대 빈도수를 중심으로 크기가 결정된다.
# 첫번째 값은 단어리스트, 두번째는 노출빈도수 나머지는 옵션이다.
# 단어들의 색 변환
wordcloud(word, frequency, colors=rainbow(length(word)), random.order=F, random.color = F)

# 다양한 단어 색 출력을 위한 팔레트 패키지의 활용
# install.packages("RColorBrewer")
# library(RColorBrewer)

pal2 = brewer.pal(8,"Dark2")
wordcloud(word, frequency, colors =pal2)

