CompanyData <- read.csv(file.choose())
View(CompanyData)
summary(CompanyData)

library(randomForest)
library(MASS)
library(caret)

range(CompanyData$Sales)

set.seed(123)

hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
boxplot(CompanyData$Sales)

highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD <- data.frame(CompanyData[2:11], highsales)
str(CD)
summary(CD$highsales)

# Data Partition
set.seed(123)
intraininglocal<- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[intraininglocal==1,]
test  <- CD[intraininglocal==2,]

set.seed(213)
rf <- randomForest(highsales~., data=train)
rf
attributes(rf)

# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)

# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$highsales)   # 100 % accuracy on training data 

# more than 95% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) # 84.35 % accuracy on test data 

plot(rf)

hist(treesize(rf), main = "No of Nodes for the trees", col = "green")
# Majority of the trees has an average number of 45 to 50 nodes. 

# Variable Importance :
varImpPlot(rf)

varImpPlot(rf ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf)

varUsed(rf)   # which predictor variables are actually used in the random forest.

# Partial Dependence Plot 
partialPlot(rf, train, Price, "Yes")
# On that graph, i see that if the price is 100 or greater, than they are not buying those computers.

# Extract single tree from the forest :
getTree(rf, 1, labelVar = TRUE)
