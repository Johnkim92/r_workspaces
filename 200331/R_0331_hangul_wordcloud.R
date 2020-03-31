library(KoNLP)
library(RColorBrewer)
library(wordcloud)
useSejongDic()

pal2 <- brewer.pal(8,"Dark2")
text <- readLines(file.choose())
text

noun <- sapply(text, extractNoun, USE.NAMES = F)
noun

noun2 <- unlist(noun) # unlist: 리스트형식을 백터 형식으로 변환
noun2

word_count <- table(noun2)
word_count

head(sort(word_count, decreasing=TRUE), 10)
wordcloud(names(word_count), freq=word_count, scale=c(6,0.3), min.freq = 3, random.order = F, rot.per = .1, colors = pal2)
