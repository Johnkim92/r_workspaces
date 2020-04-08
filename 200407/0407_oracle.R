####################################################################################
# R에서 오라클 데이터 읽어오기
####################################################################################

# R에서 데이터베이스에 접속해서 데이터를 가져오는 방법은 2가지 정도.

# 하나는 자바의 기능을 이용하는 것. 
# 또 다른 하나는 다른 언어의 기능을 사용하지 않고
# 순수 R의 패키지를 이용하는 방법.

# R에서는 Select구문을 실행하는 경우 바로 data.frame으로 리턴.
# 관계형 데이터베이스의 데이터를 사용하는 것이
# 일반적인 프로그래밍언어보다 편리하다.

# 자바의 JDBC를 이용하기 위한 패키지 설정
install.packages("RJDBC")
install.packages("igraph")

# 라이브러리 등록
library(RJDBC)
library(rJava)
library(igraph)

# 작업 디렉토리 안에 data 디렉토리에 ojdbc6.jar 파일이 존재
jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "../drivers/ojdbc6.jar")

# 데이터 베이스 연결
con <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@localhost:1521:XE", "ksk", "162534")

# 데이터 베이스 연결 종료
# dbDisconnect(con)

# dbGetQuery나 dbSendQuery를 이용해서
# 첫번째 매개변수로는 데이터베이스 연결 변수를 주고
# 두번재 매개변수로 select 구문을 주면 데이터를 가져온다.

# dbGetQuery는 data.frame을 리턴하고
# dbGHetQuery는 무조건 모든 데이터를 가져와서
# data.frame을 만들기 때문에 많은 양의 데이터가 검색된 경우
# 메모리 부족현상을 일으킬 수 있다.

# 이런 경우에는 dbSendQuery를 이용해서
# 데이터에 대한 포인터만 가져온 후
# fetch(커서, n=1)을 이용해서
# n 값으로 데이터 개수를 대입해서 필요한 만큼 데이터만 가져와서
# data.frame을 만들 수 있다.

# cursor는
# 데이터를 주고 다음으로 자동으로 넘어가는 특징을 가지고 있다.
# 하지만 전진만 하기 때문에 한번 읽은 데이터를 다시 읽지 못한다.

# select 구문을 실행하고 저장하기
tab <- dbGetQuery(con, "select * from tab")
result <- dbGetQuery(con, "select * from springboard")

# 결과 확인
class(result)

# 실행 결과를 가지고 그래프를 그릴 수 있는 프레임으로 변환
g <- graph.data.frame(result, directed = T)

# 관계도 작성
plot(g, layout=layout.fruchterman.reingold,vertext.size=8, edge.array.size=0.5)

# 오라클 쿼리 실행 (sqldf패키지)
# 오라클 sql쿼리문을 이용하기 위한 패키지 설정
install.packages("sqldf")

# 오라클 sql쿼리문을 이용하기 위한 라이브러리 등록
library(sqldf)

head(iris)

sqldf("select Species from iris")

# head
head(iris)
sqldf("select * from iris limit 4")

# subset
subset(iris, Species %in% c("setosa"))
sqldf("select * from iris where Species in ('setosa')")

subset(iris, Sepal.length >= 5 & Sepal.Length <= 5.2)
sqldf('select * from iris where "Sepal.Length" between 5 and 5.2')

