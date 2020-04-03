# 필요한 패키지들
library(devtools)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)
library(tm)
library(wordcloud2)

# 특정 개수 이상 추루되는 글자만 색깔을 변경하여 나타나도록....
# https://html-color-codes.info/Korean/

# 사이값 지정시: (weight > 800 && weight < 1000)
# 100개 이상 검색될 시 노랑으로, 아니면 초록으로 표현한다.
In_out_colors = "function(word, weight) {return (weight > 100) ? '#B09BF0' : '#61EBCB' }"

# 3. 워드클라우드2 그리기(기본)
wordcloud2(top_40)

# 3.1 wordcloud2 크기, 색 변경(size, color)
wordcloud2(top_40, size=0.5, col="random-dark")

# 3.2 키워드 회전 정도 조절(rotateRatio)
wordcloud2(top_40, size=0.5, col="random-dark", rotateRatio=0)

# 3.3 배경 색 검정(backgroundColor)
wordcloud2(top_40, size=0.5, col="random-light", backgroundColor = "black")

# 기존 모형으로 wordcloud2 생성
# 모양 선택 : shape = 'circle', 'cardioid', 'diamond','triangle-forward', 'triangle','pentagon','star'

wordcloud2(df_twords,
           shape='octagon',
           size=0.7,
           color = htmlwidgets::JS(In_out_colors),
           backgroundColor = "#D1E3E3")

# 워드클라우드 2 관련 사용방법 등 주소
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html