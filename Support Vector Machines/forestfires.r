library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
forestfire <- read.csv(file.choose())
str(forestfire)
summary(forestfire) # Confirms on the different scale and demands normalizing the data.
View(forestfire)
# The area value has lots of zeros
hist(forestfire$area)
rug(forestfire$area)
# Transform the Area value to Y 
forestfire1 <- mutate(forestfire, y = log(area + 1))  # default is to the base e, y is lower case
hist(forestfire1$y)
# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfire$temp = normalize(forestfire$temp)
forestfire$RH   = normalize(forestfire$RH)
forestfire$wind = normalize(forestfire$wind)
forestfire$rain = normalize(forestfire$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :
attach(forestfire)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfire), replace = TRUE, prob = c(0.7,0.3))
forestfire_train <- forestfire[ind==1,]
forestfire_test  <- forestfire[ind==2,]

# Building model
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data=forestfire_train,kernel = "vanilladot")
model1
Area_pred <- predict(model1, forestfire_test)
mean(Area_pred==forestfire$size_category) # 73.11%

# kernel = rfdot 
model_rbfdot<-ksvm(size_category~temp+rain+wind+RH, 
             data=forestfire_train,kernel = "rbfdot")
model_rbfdot
Area_pred_rbfdot <- predict(model_rbfdot, forestfire_test)
mean(Area_pred_rbfdot==forestfire$size_category) # 72.72%

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
             data=forestfire_train,kernel = "besseldot")
model_besseldot
Area_pred_besseldot <- predict(model_besseldot, forestfire_test)
mean(Area_pred_besseldot==forestfire$size_category) # 73.11%

# kernel = polydot
model_polydot<-ksvm(size_category~temp+rain+wind+RH, 
             data=forestfire_train,kernel = "polydot")
model_polydot
Area_pred_polydot <- predict(model_polydot, forestfire_test)
mean(Area_pred_polydot==forestfire$size_category) # 73.11%

# kernel = laplacedot
model_laplacedot<-ksvm(size_category~temp+rain+wind+RH, 
             data=forestfire_train,kernel = "laplacedot")
model_laplacedot
Area_pred_laplacedot <- predict(model_laplacedot, forestfire_test)
mean(Area_pred_laplacedot==forestfire$size_category) # 72.72%

# kernel = matrix
model_anovadot<-ksvm(size_category~temp+rain+wind+RH, 
                       data=forestfire_train,kernel = "anovadot")
model_anovadot
Area_pred_anovadot <- predict(model_anovadot, forestfire_test)
mean(Area_pred_anovadot==forestfire$size_category) # 73.11%
