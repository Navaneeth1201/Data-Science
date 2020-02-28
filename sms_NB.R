install.packages("e1071")
library(e1071)
install.packages("tm")
library(tm)
library(tmap)
install.packages("tmap")
attach(sms_raw)

sms_raw$type <-factor(sms_raw$type)

convert_counts <-function(x) {
  x <-ifelse(x > 0, "YES", "No")
}
sms_corpus <- VCorpus(VectorSource(sms_raw$text))


sms_corpus_clean <-tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <-tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
