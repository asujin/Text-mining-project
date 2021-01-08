# 기본적인 API KEY
consumerKey <- "-"
consumerSecret <- "-"
accessToken <- "-"
accessTokenSecret <- "-"

# Library 설치
install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
install.packages("base64enc")
library(base64enc)
library(tidyverse)

# 트위터 인증받기
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# 넣고싶은 키워드 설정
keyword <- enc2utf8("수사권")
###공수처
##수사처
###수사권
##검찰개혁



giso[[2]]$text
# 트위터 데이터 크롤링, n = 개수, resultType = "recent" or "popular"
gongsu_new<-searchTwitter(keyword, n=2487, lang="ko", resultType="recent")
save(gongsu_new,file="공수처_new.RData")
susa<-searchTwitter(keyword,n=1000,lang="ko",resultType="recent")
save(susa,file="수사처_new.RData")
susaright_new<-searchTwitter(keyword,n=1768,lang="ko",resultType="recent")
save(susaright,file="수사권.RData")
gaehyuck_new<-searchTwitter(keyword,n=2711,lang="ko",resultType="recent")
save(gaehyuck,file="검찰개혁.RData")
load(file = '공수처.RData')
load(file='수사처.RData')
load(file='수사권.RData')
load(file='검찰개혁.RData')
load(file=)
##giso<-searchTwitter(keyword,n=500,lang="ko",resultType="recent")


positive <- read.table("positive.txt")
negative <- read.table("negative.txt")

first_df <- twitteR::twListToDF(gongsu_new)
second_df <- twitteR::twListToDF(susa)
third_df<-twitteR::twListToDF(susaright_new)
fourth_df<-twitteR::twListToDF(gaehyuck_new)
final_df <- rbind(first_df, second_df,third_df,fourth_df)
view(final_df$text)%>%head()



df <- final_df$text %>% 
  iconv("UTF-8", sub="") %>% 
  str_remove_all(pattern = '[a-z]') %>% 
  str_remove_all(pattern = '[A-Z]') %>% 
  str_remove_all(pattern = '[@,:,#,.,/,\n,?,~,(,),!,_,ㅡ,|,…,▶,&,*,‘,’,“,”,-,°,-]')%>% 
  str_remove_all(pattern = '[ㄱ-ㅣ]') %>% 
  str_remove_all(pattern = '[0-9]') %>%
  str_remove_all(pattern = "\\[") %>%
  str_remove_all(pattern = "\\]") %>%
  str_remove_all(pattern = '\\s') %>% 
  str_remove_all(pattern = '\n\n') %>%
  str_remove_all(pattern = '\n') %>%
  str_remove_all(pattern = "\"") %>%
  str_remove_all(pattern = "%") %>%
  str_remove_all(pattern = ";") %>%
  str_remove_all(pattern = "★") %>%
  str_remove_all(pattern = "공수") %>%
  str_remove_all(pattern = "페북") %>%
  str_remove_all(pattern = "처") %>%
  str_remove_all(pattern = "펌") %>%
  str_remove_all(pattern = "비리") %>%
  str_trim() %>% 
  unique()
view(df)

##사실관계를 나타내는 뉴스 제거하기(의견반영만을 위해!)



rm_news2<-dplyr::filter(second_df,second_df$isRetweet==FALSE)

###############################3
##긍정부정 scoring
library(RcppMeCab)
#result_NNP<- NNP까지 집어넣은 모형

result_NNG <- data.frame()
parse
for(i in 1:length(df)) {
  parse <- pos(df[i], format = 'data.frame') %>% filter(pos %in% c('MAG','VA','VV','NNG')) ##MM은 관형사
  parse$token <- as.character(parse$token)
  negative_c <- 0
  positive_c <- 0
  pos_cnt <- match(parse$token, positive$V1)
  neg_cnt <- match(parse$token, negative$V1)
  pos_cnt_na <- !is.na(pos_cnt)
  neg_cnt_na <- !is.na(neg_cnt)
  sum_pos <- sum(pos_cnt_na)
  sum_neg <- sum(neg_cnt_na)
  
  review <- ''
  for(j in 1:nrow(parse)) {
    review <- str_c(review, parse$token[j], ' ')
  }
  bool <- 0
  if(sum_neg > sum_pos) {
    bool <- -1
  }
  if(sum_neg < sum_pos) {
    bool <- 1
  }
  result_NNG <- rbind(result_NNG, data.frame(review, bool))
}
view(result_NNG)

save(result,file="befor_scoring.RData")
save(df,file="text.RData")
getwd()

df

#말뭉치만들기
library(tm)
corpus <- result_NNG$review %>% VectorSource() %>% VCorpus()
parsed <- pos(df, format = c("data.frame"))


# 각 문서에서 사용된 단어의 빈도수(term frequency)를 성분으로 갖는 DTM을 생성합니다. 
# 2음절 이상인 단어만 남기는 것으로 설정하였습니다.
stopwords=c("nng","수사","검찰","공수","검사","경찰","총장","개혁")
stopwords=c("nng")
dtmTf <- DocumentTermMatrix(x = corpus, 
                            control = list(removeNumbers = TRUE, 
                                           stopwords = stopwords,
                                           wordLengths = c(2, Inf)))


colnames(x = dtmTf) <- trimws(x = colnames(x = dtmTf), which = 'both')

dim(x = dtmTf)
inspect(dtmTf)
dtmTf <- removeSparseTerms(x = dtmTf, sparse = as.numeric(x = 0.99))
inspect(dtmTf)

str(dtmTf)
x=apply(dtmTf,2,sum) 
y=sort(x, decreasing = TRUE)%>%head()
barplot(y)

dtmTf <- as.matrix(dtmTf)
final <- as.data.frame(dtmTf)
str(final)

##final 의 variable-1 / obj

for(i in 1:1242) { #행의개수
  for(j in 1:179) { #열의 개수
    if(final[i, j]>1) {
      final[i,j] = 1
    }
  }
}
final$pos_neg <- result_NNG$bool
final <- sapply(final, as.factor) %>% as.data.frame()
str(final)

install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
set.seed(321)


index <- createDataPartition(final$pos_neg, p = 0.7, list = FALSE)

# train data 할당
train <- final[index, ]
nrow(train)
# test data 할당
test  <- final[-index, ]
nrow(test)
train$pos_neg %>% table() %>% prop.table()
test$pos_neg  %>% table() %>% prop.table()


fitNB <- naiveBayes(formula = pos_neg ~ ., data = train, laplace = 0)

PredNB <- predict(object = fitNB, newdata = test)

confusionMatrix(data = PredNB, reference = test$pos_neg)

view(result)
df


##### svm
library(e1071)
fitSVM <- svm(formula = pos_neg ~ ., data = train, probability = TRUE)
tePred <- predict(object = fitSVM, newdata = test)
teReal <- test$pos_neg


confusionMatrix(data = tePred, reference = teReal)



####로지스틱
library(nnet)
fitLRm1 <- multinom(formula = pos_neg ~ ., data = train)

pred <- predict(fitLRm1, newdata = test)
real <- test$pos_neg


# 다항 로지스틱 회귀모형의 분류 성능을 확인을 위해 혼동행렬을 출력합니다. 
caret::confusionMatrix(data = pred, reference = real)

a=real %>% table() %>% prop.table()
barplot(a)
test$pos_neg  %>% table() %>% prop.table()

####random forest

library(randomForest)
str(train) ##train set의 variable
fitRFC <- randomForest(x = train[, -180], 
                       y = train[, 180], 
                       xtest = test[, -180], 
                       ytest = test[, 180], 
                       ntree = 1000, 
                       mtry = 3, 
                       importance = TRUE, 
                       do.trace = 50, # 50개마다 보여주기 TRUE면 1~1000
                       keep.forest = TRUE)




tePred <- fitRFC$test$predicted
teReal <- test$pos_neg
# 혼동행렬을 출력합니다. 
confusionMatrix(data = tePred, reference = teReal)





###연관성
rm_retweet<-dplyr::filter(final_df,final_df$isRetweet==FALSE)
view(rm_retweet)
new_df <- rm_retweet$text %>% 
  iconv("UTF-8", sub="") %>% 
  str_remove_all(pattern = '[a-z]') %>% 
  str_remove_all(pattern = '[A-Z]') %>% 
  str_remove_all(pattern = '[@,:,#,.,/,\n,?,~,(,),!,_,ㅡ,|,…,▶,&,*,‘,’,“,”,-,°,-]')%>% 
  str_remove_all(pattern = '[ㄱ-ㅣ]') %>% 
  str_remove_all(pattern = '[0-9]') %>%
  str_remove_all(pattern = "\\[") %>%
  str_remove_all(pattern = "\\]") %>%
  str_remove_all(pattern = '\\s') %>% 
  str_remove_all(pattern = '\n\n') %>%
  str_remove_all(pattern = '\n') %>%
  str_remove_all(pattern = "\"") %>%
  str_remove_all(pattern = "%") %>%
  str_trim() %>%
  unique()

view(new_df)


result_asso <- data.frame()
for(i in 1:length(new_df)) {
  parse <- pos(new_df[i], format = 'data.frame') %>% filter(pos %in% c('NNG')) ##'va','vv'#MAG삭제
  parse$token <- as.character(parse$token)
  negative_c <- 0
  positive_c <- 0
  pos_cnt <- match(parse$token, positive$V1)
  neg_cnt <- match(parse$token, negative$V1)
  pos_cnt_na <- !is.na(pos_cnt)
  neg_cnt_na <- !is.na(neg_cnt)
  sum_pos <- sum(pos_cnt_na)
  sum_neg <- sum(neg_cnt_na)
  
  review <- ''
  for(j in 1:nrow(parse)) {
    review <- str_c(review, parse$token[j], ' ')
  }
  bool <- 0
  if(sum_neg > sum_pos) {
    bool <- -1
  }
  if(sum_neg < sum_pos) {
    bool <- 1
  }
  result_asso <- rbind(result, data.frame(review, bool))
}
view(result_asso)
corpus_related <- result_asso$review %>% VectorSource() %>% VCorpus()




library(tm)

# 키워드를 설정합니다. 
keyword <- '개혁'

stopwords=c("아직","지금","사람","이재명","간첩","윤웅걸","총장","지명","차기","나오","윤웅걸","조작","언론","증거","신설","지검")
dtmTf <- DocumentTermMatrix(x = corpus_related, 
                            control = list(removeNumbers = TRUE, 
                                           stopwords = stopwords,
                                           wordLengths = c(2, Inf)))
dtmTf <- removeSparseTerms(x = dtmTf, sparse = as.numeric(x = 0.99))
inspect(dtmTf)

# dtmTf에서 키워드를 포함하는 단어를 추출합니다. 상위 10게만 출력합니다. 
findAssocs(x = dtmTf, terms = keyword, corlimit = 0.01) %>% `[[`(1) %>% head(n = 10L)

# 사용자 정의 함수로 만들어봅니다. 
getAssocs <- function(dtm, keyword, corlimit = 0.01) {
  
  # 상관계수가 높은 단어를 추출합니다.
  assocs <- findAssocs(x = dtm, terms = keyword, corlimit = corlimit) %>% `[[`(1)
  
  # 길이가 10개 이상이면 10개까지만 출력합니다. 
  if (length(x = assocs) >= 10) {
    assocs <- assocs[1:10]
  }
  
  return(assocs)
  
}
view(new_df)

# 관심 있는 단어들을 입력하여 연관성 높은 단어들을 확인해봅니다. 
getAssocs(dtm = dtmTf, keyword = '반대')



# getAssocs 실행 결과를 저장합니다. 
assocs <- getAssocs(dtm = dtmTf, keyword = '반대')

#
# getAssocs 결과 객체로 막대그래프를 그립니다. 
bp <- barplot(height = assocs, 
              space = 0.5, 
              axes = FALSE, 
              ylim = c(0, max(assocs)*1.1),
              main = '왜 공수처에 반대할까?')

# 상관계수를 추가합니다. 
text(x = bp, 
     y = assocs, 
     labels = assocs, 
     pos = 3, 
     cex = 1.0, 
     font = 2) 



barplot(a,legend.text = c("-1=>부정","0=>중립","1=>긍정"),col=c(2,3,4))

install.packages("igraph")
library(igraph)

treemap(
  dtf = wordDf[1:300, ],
  title = '고빈도 단어 트리맵',
  index = c('word'),
  vSize = 'freq', 
  fontsize.labels = 14,
  palette = myPal,
  border.col = 'white')

wordcloud2(
  data = wordDf[1:15, ],
  size = 0.5, 
  color = myPal,
  backgroundColor = 'white',
  shape = 'circle', 
  ellipticity = 0.75, 
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.25)

wordsFreq <- dtmTf %>% as.matrix() %>% colSums()

# 소수점 첫째 자리에서 반올림합니다. 
wordsFreq <- round(x = wordsFreq, digits = 2L)

# 사용된 단어의 총 개수를 확인합니다.
length(x = wordsFreq)

# 단어 빈도수(TF) 기준으로 내림차순 정렬하고, 상위 20개만 출력합니다.
wordsFreq <- wordsFreq[order(wordsFreq, decreasing = TRUE)]
head(x = wordsFreq, n = 20L)

# 단어 빈도를 막대그래프로 그리기 위해 데이터 프레임으로 변환한 다음 
# 내림차순으로 정렬합니다.

wordDf <- data.frame(word = names(x = wordsFreq), 
                     freq = wordsFreq, 
                     row.names = NULL) %>% 
  arrange(desc(x = freq))

# 건수를 확인합니다.
nrow(x = wordDf)


# 그래프 창 설정을 변경합니다. 
par(family = 'NanumGothic', mar = c(5, 0, 4, 0))

# 고빈도 상위 20개 단어로만 그립니다. 
highFreq <- wordDf[1:10, ]

# wordDf 객체로 막대그래프를 그립니다. 
bp <- barplot(height = highFreq$freq, 
              names.arg = highFreq$word, 
              space = 0.5, 
              axes = FALSE, 
              ylim = c(0, max(highFreq$freq)*1.1),
              main = '고빈도 단어')

# 상관계수를 추가합니다. 
text(x = bp, 
     y = highFreq$freq, 
     labels = highFreq$freq, 
     pos = 3, 
     cex = 1.0, 
     font = 2) 
