data <- read.csv(file.choose())
data<-data[c(3060:4060),c(4,5)]
str(data)

library(dplyr)
library(caret)
library(e1071)
library(tm)
library(SnowballC)
library(tm.plugin.mail)
library(stringr)
library(tidytext)
library(pander)

corpus = VCorpus(VectorSource(data))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.95)

train_index <- createDataPartition(data$Class, p=0.70, list=FALSE)


email_raw_train <- data[-train_index,]
email_corpus_clean_train <- corpus[-train_index]
email_dtm_train <- dtm[-train_index,]

email_raw_test <- data[-train_index,]
email_corpus_clean_test <- corpus[-train_index]
email_dtm_test <- dtm[-train_index,]

#convert counts to a factor 

convert_counts<- function(x) {
  
  x <- ifelse(x > 0,1,0)
  x <- factor(x, levels = c(0, 1), labels = c("No","Yes"))
}

#Create a dictionary of words that appear at least 5 times in the emails.
email_dict <- findFreqTerms(email_dtm_train, lowfreq=5)

email_train <- DocumentTermMatrix(email_corpus_clean_train, list(dictionary=email_dict))
email_train <- email_train %>% apply(MARGIN=2, FUN=convert_counts)

email_test <- DocumentTermMatrix(email_corpus_clean_test, list(dictionary=email_dict))
email_test <- email_test %>% apply(MARGIN=2, FUN=convert_counts)

ctrl <- trainControl(method="cv", 10)
set.seed(12358)
email_model2 <- train(email_train, email_raw_train$Class, method="nb", 
                      tuneGrid=data.frame(.fL=1, .usekernel=FALSE, .adjust = seq(0, 5, by = 1)),
                      trControl=ctrl)
setwd("C:\\Users/ABC/Documents/")
save(email_model2,file='email_model2.rds')
email_model2

email_predict2 <- predict(email_model2, email_test)
cm2 <- confusionMatrix(email_predict2, email_raw_test$Class, positive="Abusive")
cm2




