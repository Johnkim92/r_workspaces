useNIADic()

practice_txt <- readLines("../datas/remake.txt")

practice_txt <- str_replace_all(practice_txt, "\\W", " ")

pr_nouns <- extractNoun(practice_txt)

wordcounts <- table(unlist(pr_nouns))

df_words <- as.data.frame(wordcounts, stringsAsFactors = F)
df_words <- rename(df_words, word=Var1, freq = Freq)
df_words <- filter(df_words, nchar(word) >= 2)

tops_20 <- df_words %>% 
  arrange(desc(freq)) %>% 
  head(20)
set.seed(1234)
wordcloud(words = tops_20$word,
          freq = tops_20$freq,
          min.freq = 2,
          max.words = 200,
          rot.per = .1,
          random.order = F,
          scale = c(4,0.3),
          colors = pal)
