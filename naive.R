str(sms_raw_NB)
table(sms_raw_NB$type)
install.packages("NLP")
library(tm)

sms_corpous<-Corpus(VectorSource(sms_raw_NB$text))
sms_corpous$content[1:10]


#Cleaning data
corpus_clean<-tm_map(sms_corpous, tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean, removeNumbers, stopwords())
corpus_clean<-tm_map(corpus_clean, removePunctuation)
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*", "",x)
corpus_clean<-tm_map(corpus_clean,content_transformer(removeNumPunct))
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
corpus_clean$content[1:10]

?tm_map
?stopwords

sms_dtm<- DocumentTermMatrix(corpus_clean)
class(sms_dtm)

as.character(sms_dtm)

#creating train and test datasets.
sms_raw_train<-sms_raw_NB[1:4169,]
sms_raw_test<-sms_raw_NB[4170:5559,]

sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]

#Checking proportion of spam that it is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Idicator features for frequent word
sms_dict<-findFreqTerms(sms_dtm_train,5)
list(sms_dict[1:100])

?findFreqTerms
sms_train<-DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_dict

#convert counts to  factor
convert_counts<- function(x)  
{
  x<- ifelse(x>0,1,0)
  x<- factor(x, levels=c(0,1), labels = c("NO", "Yes"))
}

sms_train<-apply(sms_train, MARGIN = 2, convert_counts)
sms_test<-apply(sms_test,  MARGIN = 2, convert_counts)

?apply
View(sms_train)
View(sms_test)

#Traning model on data set

library(e1071)
sms_classifier<- naiveBayes(sms_train, sms_raw_train$type)


sms_classifier$levels
sms_classifier$tables

#Evalutaing model performance
sms_test_pred<- predict(sms_classifier, sms_test)
sms_test_pred[1:25]

table1<- table(sms_test_pred,sms_raw_test$type)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, 
           prop.chisq=FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

sms_classifier2<-naiveBayes(sms_train, sms_raw_train$type, laplace = 4)
sms_test_pred2<- predict(sms_classifier2, sms_test)

?naiveBayes()

table2<- table(sms_test_pred2, sms_raw_test$type)

CrossTable(sms_test_pred, sms_raw_test$type, 
           prop.chisq=FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#Accuracy

accuracy1<-(sum(diag(table1))/sum(table1))
accuracy1

accuracy2<-(sum(diag(table2))/sum(table2))
accuracy2


mean(sms_test_pred==sms_raw_test$type)


mean(sms_test_pred2==sms_raw_test$type)


