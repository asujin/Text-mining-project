# �⺻���� API KEY
consumerKey <- "-"
consumerSecret <- "-"
accessToken <- "-"
accessTokenSecret <- "-"

# Library ��ġ
install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
install.packages("base64enc")
library(base64enc)
library(tidyverse)

# Ʈ���� �����ޱ�
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# �ְ����� Ű���� ����
keyword <- enc2utf8("�����")
###����ó
##����ó
###�����
##��������



giso[[2]]$text
# Ʈ���� ������ ũ�Ѹ�, n = ����, resultType = "recent" or "popular"
gongsu_new<-searchTwitter(keyword, n=2487, lang="ko", resultType="recent")
save(gongsu_new,file="����ó_new.RData")
susa<-searchTwitter(keyword,n=1000,lang="ko",resultType="recent")
save(susa,file="����ó_new.RData")
susaright_new<-searchTwitter(keyword,n=1768,lang="ko",resultType="recent")
save(susaright,file="�����.RData")
gaehyuck_new<-searchTwitter(keyword,n=2711,lang="ko",resultType="recent")
save(gaehyuck,file="��������.RData")
load(file = '����ó.RData')
load(file='����ó.RData')
load(file='�����.RData')
load(file='��������.RData')
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
  str_remove_all(pattern = '[@,:,#,.,/,\n,?,~,(,),!,_,��,|,��,��,&,*,��,��,��,��,-,��,-]')%>% 
  str_remove_all(pattern = '[��-��]') %>% 
  str_remove_all(pattern = '[0-9]') %>%
  str_remove_all(pattern = "\\[") %>%
  str_remove_all(pattern = "\\]") %>%
  str_remove_all(pattern = '\\s') %>% 
  str_remove_all(pattern = '\n\n') %>%
  str_remove_all(pattern = '\n') %>%
  str_remove_all(pattern = "\"") %>%
  str_remove_all(pattern = "%") %>%
  str_remove_all(pattern = ";") %>%
  str_remove_all(pattern = "��") %>%
  str_remove_all(pattern = "����") %>%
  str_remove_all(pattern = "���") %>%
  str_remove_all(pattern = "ó") %>%
  str_remove_all(pattern = "��") %>%
  str_remove_all(pattern = "��") %>%
  str_trim() %>% 
  unique()
view(df)

##��ǰ��踦 ��Ÿ���� ���� �����ϱ�(�ǰ߹ݿ����� ����!)



rm_news2<-dplyr::filter(second_df,second_df$isRetweet==FALSE)

###############################3
##�������� scoring
library(RcppMeCab)
#result_NNP<- NNP���� ������� ����

result_NNG <- data.frame()
parse
for(i in 1:length(df)) {
  parse <- pos(df[i], format = 'data.frame') %>% filter(pos %in% c('MAG','VA','VV','NNG')) ##MM�� ������
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

#����ġ�����
library(tm)
corpus <- result_NNG$review %>% VectorSource() %>% VCorpus()
parsed <- pos(df, format = c("data.frame"))


# �� �������� ���� �ܾ��� �󵵼�(term frequency)�� �������� ���� DTM�� �����մϴ�. 
# 2���� �̻��� �ܾ ����� ������ �����Ͽ����ϴ�.
stopwords=c("nng","����","����","����","�˻�","����","����","����")
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

##final �� variable-1 / obj

for(i in 1:1242) { #���ǰ���
  for(j in 1:179) { #���� ����
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

# train data �Ҵ�
train <- final[index, ]
nrow(train)
# test data �Ҵ�
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



####������ƽ
library(nnet)
fitLRm1 <- multinom(formula = pos_neg ~ ., data = train)

pred <- predict(fitLRm1, newdata = test)
real <- test$pos_neg


# ���� ������ƽ ȸ�͸����� �з� ������ Ȯ���� ���� ȥ������� ����մϴ�. 
caret::confusionMatrix(data = pred, reference = real)

a=real %>% table() %>% prop.table()
barplot(a)
test$pos_neg  %>% table() %>% prop.table()

####random forest

library(randomForest)
str(train) ##train set�� variable
fitRFC <- randomForest(x = train[, -180], 
                       y = train[, 180], 
                       xtest = test[, -180], 
                       ytest = test[, 180], 
                       ntree = 1000, 
                       mtry = 3, 
                       importance = TRUE, 
                       do.trace = 50, # 50������ �����ֱ� TRUE�� 1~1000
                       keep.forest = TRUE)




tePred <- fitRFC$test$predicted
teReal <- test$pos_neg
# ȥ������� ����մϴ�. 
confusionMatrix(data = tePred, reference = teReal)





###������
rm_retweet<-dplyr::filter(final_df,final_df$isRetweet==FALSE)
view(rm_retweet)
new_df <- rm_retweet$text %>% 
  iconv("UTF-8", sub="") %>% 
  str_remove_all(pattern = '[a-z]') %>% 
  str_remove_all(pattern = '[A-Z]') %>% 
  str_remove_all(pattern = '[@,:,#,.,/,\n,?,~,(,),!,_,��,|,��,��,&,*,��,��,��,��,-,��,-]')%>% 
  str_remove_all(pattern = '[��-��]') %>% 
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
  parse <- pos(new_df[i], format = 'data.frame') %>% filter(pos %in% c('NNG')) ##'va','vv'#MAG����
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

# Ű���带 �����մϴ�. 
keyword <- '����'

stopwords=c("����","����","���","�����","��ø","������","����","����","����","����","������","����","���","����","�ż�","����")
dtmTf <- DocumentTermMatrix(x = corpus_related, 
                            control = list(removeNumbers = TRUE, 
                                           stopwords = stopwords,
                                           wordLengths = c(2, Inf)))
dtmTf <- removeSparseTerms(x = dtmTf, sparse = as.numeric(x = 0.99))
inspect(dtmTf)

# dtmTf���� Ű���带 �����ϴ� �ܾ �����մϴ�. ���� 10�Ը� ����մϴ�. 
findAssocs(x = dtmTf, terms = keyword, corlimit = 0.01) %>% `[[`(1) %>% head(n = 10L)

# ����� ���� �Լ��� �����ϴ�. 
getAssocs <- function(dtm, keyword, corlimit = 0.01) {
  
  # �������� ���� �ܾ �����մϴ�.
  assocs <- findAssocs(x = dtm, terms = keyword, corlimit = corlimit) %>% `[[`(1)
  
  # ���̰� 10�� �̻��̸� 10�������� ����մϴ�. 
  if (length(x = assocs) >= 10) {
    assocs <- assocs[1:10]
  }
  
  return(assocs)
  
}
view(new_df)

# ���� �ִ� �ܾ���� �Է��Ͽ� ������ ���� �ܾ���� Ȯ���غ��ϴ�. 
getAssocs(dtm = dtmTf, keyword = '�ݴ�')



# getAssocs ���� ����� �����մϴ�. 
assocs <- getAssocs(dtm = dtmTf, keyword = '�ݴ�')

#
# getAssocs ��� ��ü�� ����׷����� �׸��ϴ�. 
bp <- barplot(height = assocs, 
              space = 0.5, 
              axes = FALSE, 
              ylim = c(0, max(assocs)*1.1),
              main = '�� ����ó�� �ݴ��ұ�?')

# �������� �߰��մϴ�. 
text(x = bp, 
     y = assocs, 
     labels = assocs, 
     pos = 3, 
     cex = 1.0, 
     font = 2) 



barplot(a,legend.text = c("-1=>����","0=>�߸�","1=>����"),col=c(2,3,4))

install.packages("igraph")
library(igraph)

treemap(
  dtf = wordDf[1:300, ],
  title = '���� �ܾ� Ʈ����',
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

# �Ҽ��� ù° �ڸ����� �ݿø��մϴ�. 
wordsFreq <- round(x = wordsFreq, digits = 2L)

# ���� �ܾ��� �� ������ Ȯ���մϴ�.
length(x = wordsFreq)

# �ܾ� �󵵼�(TF) �������� �������� �����ϰ�, ���� 20���� ����մϴ�.
wordsFreq <- wordsFreq[order(wordsFreq, decreasing = TRUE)]
head(x = wordsFreq, n = 20L)

# �ܾ� �󵵸� ����׷����� �׸��� ���� ������ ���������� ��ȯ�� ���� 
# ������������ �����մϴ�.

wordDf <- data.frame(word = names(x = wordsFreq), 
                     freq = wordsFreq, 
                     row.names = NULL) %>% 
  arrange(desc(x = freq))

# �Ǽ��� Ȯ���մϴ�.
nrow(x = wordDf)


# �׷��� â ������ �����մϴ�. 
par(family = 'NanumGothic', mar = c(5, 0, 4, 0))

# ���� ���� 20�� �ܾ�θ� �׸��ϴ�. 
highFreq <- wordDf[1:10, ]

# wordDf ��ü�� ����׷����� �׸��ϴ�. 
bp <- barplot(height = highFreq$freq, 
              names.arg = highFreq$word, 
              space = 0.5, 
              axes = FALSE, 
              ylim = c(0, max(highFreq$freq)*1.1),
              main = '���� �ܾ�')

# �������� �߰��մϴ�. 
text(x = bp, 
     y = highFreq$freq, 
     labels = highFreq$freq, 
     pos = 3, 
     cex = 1.0, 
     font = 2) 