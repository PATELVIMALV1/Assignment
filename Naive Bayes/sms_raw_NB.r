library(rmarkdown)
library(tm)
library(e1071)
library(gmodels)
library(caret)
sms_data<-read.csv(file.choose(),stringsAsFactors = F)
class(sms_data)
View(sms_data)
summary(sms_data)
str(sms_data)
sms_data$type<-as.factor(sms_data$type)
str(sms_data)
table(sms_data$type)

# Prepare corpuse for the text data 
# Corpus - it is a collection of documents:
# Vcorpus is a volatile corpus which stores the corpus in memory and would be destroyed 
# once when the R object gets destroyed.
# Pcorpus is a permanent corpus that is stored outside of memory(in database)
# corpus uses simple corpus which does not allow you to keep dashes, underscores or punctuations.

sms_corpous<-VCorpus(VectorSource(sms_data$text))
class(sms_corpous)

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(sms_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
class(corpus_clean)

# create a document-term sparse matrix
# Convert into a plain text document before converting to Document Term Matrix.
# Why to convert to DTM
# In text mining, it is important to create the document-term matrix (DTM) of the corpus we are interested in. A DTM is basically a matrix, with 
# documents designated by rows and words by columns, that the elements are the counts or the weights.

sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)

# creating training and test datasets
sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator features for frequent words
# if the word has been referred to 5 times or more
sms_dict<-findFreqTerms(sms_dtm, 5)

# Now apply this particular dictionary of words to training and testing data.
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))



temp <- as.data.frame(as.matrix(sms_train))


View(temp)
dim(sms_train)
dim(sms_test)

#inspect(sms_corpus_train[1:100])
#list(sms_dict[1:100])
# convert counts to a factor
# Create a custom function to show that if a specific word as been used more than once.
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)

##  Training a model on the data ----
# Now apply naiveBayes Model on the new sms_train and original data
# on Type (Classifier for ham or spam)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))

confusionMatrix(sms_test_pred,sms_raw_test$type) # 97.7%