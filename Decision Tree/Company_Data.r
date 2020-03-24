install.packages("partykit")
library(partykit)
library(C50)
install.packages("tree")
library(tree)
install.packages("gmodels")
library(gmodels)
library(caret)
CompanyData <- read.csv(file.choose())
View(CompanyData)
summary(CompanyData)

hist(CompanyData$Sales)

range(CompanyData$Sales)
High<-ifelse(CompanyData$Sales<9, "No", "Yes")

CD<-data.frame(CompanyData, High)

CD_train <- CD[1:200,]
CD_test <- CD[201:400,]
#Using Party Function 
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(op_tree)
plot(op_tree)
# On looking into the Above tree, i see that if the Location of the Shelv is good,
# then there is a probability of 70% chance that the customer will buy.
# With ShelveLoc having a Bad or Medium and Price <= 90, the probability of High sales 
# could be 60%.
# If ShelveLoc is Bad or Medium, With Price >= 87 and Advertising less then <= 6 then there
# is a zero percent chance of high sales.
# If ShelveLoc is Bad or Medium, With Price >= 87 and Advertising greater then > 7 then there
# is a 20 % percent chance of high sales.

pred_test <- predict(op_tree,newdata=CD_test)
pred_test
CrossTable(CD_test$High,pred_test)
confusionMatrix(CD_test$High,pred_test) # Accuracy = 76%

##### Using tree function 
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_1 <- predict(cd_tree,newdata=CD_test)
pred_tree$final <- colnames(pred_test_1)[apply(pred_test_1,1,which.max)]
summary(CD_test$High)
pred_tree$final <- as.factor(pred_tree$final)
CrossTable(CD_test$High,pred_tree$final)
confusionMatrix(CD_test$High,pred_tree$final)

##### Using C5.0 function
intraininglocal<-createDataPartition(CD$High, p = .75, list = F)

training<-CD[intraininglocal,]
testing<-CD[-intraininglocal,]
View(training)

model<-C5.0(training$High~., data = training[,-1])  #Trial - Boosting parameter
plot(model)
#Generate the model summary
summary(model)
#Predict for test data set
pred<- predict.C5.0(model, newdata=testing[,-1])
CrossTable(testing$High,pred)
confusionMatrix(testing$High,pred) #Accuracy is 83.84%

#Model 2

model2<-C5.0(training$High~., data = training[,-1],trials=100)  #Trial - Boosting parameter
plot(model2)
pred2<- predict.C5.0(model2, newdata=testing[,-1])
CrossTable(testing$High,pred2)
confusionMatrix(testing$High,pred2) #Accuracy is 83.64%
plot(Model2,cex=0.5)
