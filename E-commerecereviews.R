library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
aurl <- "https://www.amazon.com/MSI-GT63-TITAN-052-Extreme-i7-8750H/product-reviews/B07CSFW5Y1/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
setwd("C:/Users/naveen/downloads")
write.table(amazon_reviews,"MSIGTGN.txt",row.names = F)


MSIGTGN_Lap <- read.delim('MSIGTGN.TXT')
str(MSIGTGN_Lap)
View(MSIGTGN_Lap)
library(tm)
corpus <- MSIGTGN_Lap[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
letterCloud(w,word = 'Am',frequency(5), size=1)
# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
Amzn_reviews <- read.delim('MSIGTGN.TXT')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('Love')
get_nrc_sentiment('glaring')
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        -MSI GT63 TITAN Gaming Laptop')