# ��Ű�� �ε�
## ��ġ�Ǿ��ִ� ��쿡�� �̺κ� ����
# install.packages("pacman")
pacman::p_load('tidymodels','tidytext','NLP4kec', 'stringr','magrittr','tm', 'network','GGally', 'sna', 'RColorBrewer')

# ������ �ε�
## gender_conflict_title_2013
riss_geder_conflict_title_2013 <- read.csv('riss_gender_conflict_rawdata/gender_conflict_title_2013.csv')
RGCT_2013_txt <- as.data.frame(riss_geder_conflict_title_2013, stringsAsFactors = F)

# �ؽ�Ʈ ��ó��
# �ؽ�Ʈ�� �ߺ��� �� ����
RGCT_2013_txt <- unique(RGCT_2013_txt)

# �ؽ�Ʈ�� ���� ����
RGCT_2013_txt <- sapply(RGCT_2013_txt,str_remove_all,'\\s+')
RGCT_2013_txt <- as.data.frame(RGCT_2013_txt,stringsAsFactors = FALSE)
colnames(RGCT_2013_txt) <- c('content')

# �� �̸��� content�� id�� ����
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

# ����� ���� �ҿ�� ����
delDic <- readLines("stopword.txt", warn = FALSE)

# ���¼� �м�(NLP4kec ��Ű��)
Parsed_RGCT_2013 <- r_parser_r(RGCT_2013_txt$content,language = "ko")
Parsed_RGCT_2013 <- Parsed_RGCT_2013[Parsed_RGCT_2013 != ""]

# corpus ����
corp <- VCorpus(VectorSource(Parsed_RGCT_2013))

# ���� ����
corp <- tm_map(corp, removeNumbers)
# Ư������ ����
corp <- tm_map(corp, removePunctuation)
# �ҿ�� ����
corp <- tm_map(corp, removeWords, words = delDic)

# DTM/TDM
# dtmTfIdf
dtmTfIdf <- DocumentTermMatrix( x = corp, control = list( removeNumbers = TRUE, wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = TRUE) ))  

# dtmTfIdf ���� ���
dtmTfIdf <- removeSparseTerms(x =  dtmTfIdf, sparse = as.numeric(x = 0.99))

# corTerms
dtmTfIdf %>% as.matrix() %>% cor() -> corTerms

# ��Ʈ��ũ �� �׸���
# ������ ũ�� ����
corTerms[corTerms <= 0.5] <- 0
# corTerms to network obj
netTerms <- network(x = corTerms, directed = FALSE)

# �����߽ɼ� ���
degreeTerms <- degree(netTerms) 

# �����߽ɼ� ǥ��
netTerms %v% 'mode' <-
    ifelse(
        test = degreeTerms >= quantile(x = degreeTerms, probs = 0.90, na.rm = TRUE), 
        yes = 'Top', 
        no = 'Rest')
nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')
set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 1)

# ��Ʈ��ũ �� �׸���
ggnet2(
    net = netTerms,
    mode = 'fruchtermanreingold',
    layout.par = list(cell.jitter = 0.001),
    size.min = 10,
    label = TRUE,
    label.size = 3,
    node.color = 'mode',
    palette = nodeColors,
    node.size = degreeTerms,
    edge.size = 'edgeSize',
    family = 'mono') +
    labs(title = "�����߽ɼ� �ݿ��� �ܾ�-��Ʈ��ũ��")