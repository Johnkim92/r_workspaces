###################################################################################
# "제주도 여행코스 추천" 검색어 결과를 그래프로 표시.
###################################################################################
# 단어 추가(제주도여행지.txt)를 읽어들인 후, dataframe으로 변경하여 기존 사전에 추가
# 데이터 읽어오기 (jeju.txt)
# 한글 외 삭제, 영어
# 읽어 들인 데이터로 부터 제거할 단어 리스트 읽어오기(제주도여행코스gsub.txt)
# 두 글자 이상인 단어만 추출
# 현재까지의 작업을 파일로 저장 후, 저장된 파일읽기
# 단어 빈도 수 구한 후, 워드 클라우드 작업
###################################################################################
# 가장 추천수가 많은 상위 10개 골라서
# 1. pie 그래프로 출력.
# 2. bar형태의 그래프로 표시하기
# 3. 옆으로 누운 바 그래프 그리기
# 4. 3D Pie Chart로 표현. (plotrix라는 패키지가 추가로 필요.)

useSejongDic()

mergeUserDic(data.frame(readLines("../datas/제주도여행지.txt"),"ncn")) 
txt <- readLines("../datas/jeju.txt")

place <- sapply(txt, extractNoun, USE.NAMES = F)
c <- unlist(place)
res <- str_replace_all(c, "[^[:alpha:]]", "")
res <- gsub(" ","",res)
res

txt <- readLines("../datas/제주도여행코스gsub.txt")
cnt_txt <- length(txt)

for (i in 1:cnt_txt) {
  res <- gsub((txt[i]), "",res)
}
res2 <- Filter(function(x) {nchar(x) >= 2}, res)
write(res2, "../datas/jeju2.txt")
res3 <- read.table("../datas/jeju2.txt")
wordcount <- table(res3)

palete <- brewer.pal(8,"Set2")

wordcloud(names(wordcount),freq = wordcount, scale=c(3,1), rot.per = 0.25, min.freq = 5, random.order = F, random.color = T, colors = palete)
legend(0.3,1,"제주도 여행코스 추천", cex=0.6, fill=NA, border=NA, bg="white",text.col="red",text.font=2, box.col="red")


df_word <- as.data.frame(wordcount, stringsAsFactors = F)

df_word <- rename(df_word, word=res3, freq = Freq)

top10 <- df_word %>% arrange(desc(freq)) %>% head(10) # top10 <- head(sort(wordcount, decreasing=T),10)
top10

pct <-  round(top10$freq/sum(top10$freq)*100,1) # %계산 

label <- top10$word
label <- paste(label, pct) # 라벨에 %값 넣기
label <- paste(label, "%", sep="") # 라벨에 %표시 넣어줌
# label <- paste(names(top10),"\n",pct,"%")

pie(top10$freq, init.angle=90, labels=label, main="제주도 여행코스 추천 top10", col=rainbow(10), cex=0.8)
bp <- barplot(top10$freq, names.arg=top10$word, main="제주도 여행코스 추천 top10", col=rainbow(length(top10$freq)), xlab="코스", ylab="언급 횟수", ylim=c(0,25), cex.names=0.7, las=2)
text(x=bp, y=head(sort(wordcount,decreasing=T),10)*1.05,labels=paste("(",pct,"%",")"),col="black",cex=0.7)
text(x=bp, y=head(sort(wordcount,decreasing=T),10)*0.95,labels=paste(head(sort(wordcount,decreasing=T),10), "건"), col="black", cex=0.7)
barplot(top10$freq, names.arg=top10$word, main="제주도 여행코스 추천 top10", col=rainbow(length(top10$freq)), xlab="언급 횟수", ylab="코스",horiz=TRUE, xlim=c(0,25))

library(plotrix)
pie3D(top10$freq, labels=label, explode=0.1, labelcex=0.8, main="제주도 여행코스 추천 top10")
