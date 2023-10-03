#패키지 로드
##설치되어있는 경우에는 이부분 생략
#install.packages("pacman")
pacman::p_load('tidymodels','tidytext','NLP4kec', 'stringr','magrittr','tm', 'network','GGally', 'sna', 'RColorBrewer')

#데이터 로드
##gender_conflict_title_2013
riss_geder_conflict_title_2013 <- read.csv('riss_gender_conflict_rawdata/gender_conflict_title_2013.csv')
RGCT_2013_txt <- as.data.frame(riss_geder_conflict_title_2013, stringsAsFactors = F)

#텍스트 전처리
#텍스트의 중복된 행 제거
RGCT_2013_txt <- unique(RGCT_2013_txt)

#텍스트의 공백 제거
RGCT_2013_txt <- sapply(RGCT_2013_txt,str_remove_all,'\\s+')
RGCT_2013_txt <- as.data.frame(RGCT_2013_txt,stringsAsFactors = FALSE)
colnames(RGCT_2013_txt) <- c('content')

#열 이름을 content와 id로 설정
generateIDs <- function(obj, index = 'id') {
    if (obj %>% class() == 'data.frame') {
        n <- nrow(x = obj)
    } else {
        n <- length(x = obj)
    }
    id <- str_c(
        index, 
        str_pad(
            string = 1:n, 
            width = ceiling(x = log10(x = n)), 
            side = 'left', 
            pad = '0') )
    return(id)
}  
RGCT_2013_txt$id <- generateIDs(obj = RGCT_2013_txt, index = 'doc')
names(RGCT_2013_txt) <- c("content","id")

# 사용자 정의 불용어 사전
delDic <- readLines("stopword.txt", warn = FALSE)

#형태소 분석(NLP4kec 패키지)
Parsed_RGCT_2013 <- r_parser_r(RGCT_2013_txt$content,language = "ko")
Parsed_RGCT_2013 <- Parsed_RGCT_2013[Parsed_RGCT_2013 != ""]

#corpus 생성
corp <- VCorpus(VectorSource(Parsed_RGCT_2013))

#숫자 제거
corp <- tm_map(corp, removeNumbers)
#특수문자 제거
corp <-  tm_map(corp, removePunctuation)
#불용어 제거
corp <- tm_map(corp, removeWords, words = delDic)

#DTM/TDM
#dtmTfIdf
dtmTfIdf <- DocumentTermMatrix( x = corp, control = list( removeNumbers = TRUE, wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = TRUE) ))  

# dtmTfIdf 차원 축소
dtmTfIdf <- removeSparseTerms(x =  dtmTfIdf, sparse = as.numeric(x = 0.99))

#corTerms
dtmTfIdf %>% as.matrix() %>% cor() -> corTerms
glimpse(corTerms)

#네트워크 맵 그리기
#상관행렬 크기 조정
corTerms[corTerms <= 0.55] <- 0
#corTerms to network obj
netTerms <- network(x = corTerms, directed = FALSE)
plot(netTerms, vertex.cex = 1.5)

#매개중심성 계산
btnTerms <- betweenness(netTerms) 

#매개중심성 표현
netTerms %v% 'mode' <-
    ifelse(
        test = btnTerms >= quantile(x = btnTerms, probs = 0.90, na.rm = TRUE), 
        yes = 'Top', 
        no = 'Rest')
nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')
set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 0.5)
ggnet2(
    net = netTerms,
    mode = 'fruchtermanreingold',
    layout.par = list(cell.jitter = 0.001),
    size.min = 10,
    label = TRUE,
    label.size = 3,
    node.color = 'mode',
    palette = nodeColors,
    node.size = sna::degree(dat = netTerms),
    edge.size = 'edgeSize',
    family = 'mono')+
    labs(title = "매개중심성 반영한 단어-네트워크맵")

