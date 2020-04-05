setwd("D:/Assignment/Text Mining")
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Redgear-Cosmo-7-1-Headphones-Controller/dp/B079S811J3/ref=sr_1_1?dchild=1&keywords=cosmo+headphone&qid=1586069787&s=electronics&sr=8-1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"headphone.txt",row.names = F)
length(amazon_reviews)
headphone <- read.delim('headphone.txt')
str(headphone)
View(headphone)

# Build Corpus and DTM/TDM
library(tm)
corpus <- headphone[-1,]
head(corpus)
class(corpus)
# Corpus
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
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

cleanset<-tm_map(cleanset,removeWords, c('headphone','can','gaming','headset'))
# Since the word 'headphone','can','gaming' and'headset' is used more, this can be removed as we are 
# mining the review for this headphone only.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.


cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
# the barplot pulls both Computer and Machine as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
# the terms indicate that there are 318 words and 3 documents in this TDM
# Sparsity is 67% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:3]
# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word sound and quaity as the highest frequency. This implies
# that headphone has got more reviews about the headphone quality 

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

# Sentiment Analysis for reviews:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# install.packages("syuzhet")

# Read File 
Amzn_reviews <- read.delim('headphone.txt')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)

get_nrc_sentiment('Love')

get_nrc_sentiment('glaring')

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        -MSI GT63 TITAN Gaming Laptop')
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(tdm)
#Display dtm
inspect(dtm)
rowTotals <- apply(dtm, 1, sum)
dtm.new   <- dtm[rowTotals > 0, ]
library(tm)
library(devtools)
library(slam)
library(topicmodels)
lda <- LDA(dtm.new,10 ) # find 10 topic
term <- terms(lda, 20) # first 10 terms of every topic
term
