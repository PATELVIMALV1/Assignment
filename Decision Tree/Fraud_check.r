install.packages("partykit")
library(partykit)
library(C50)
install.packages("tree")
library(tree)
install.packages("gmodels")
library(gmodels)
library(caret)
install.packages("png")
install.packages("knitr")
library(knitr)
library(png)
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
summary(FraudCheck)

hist(FraudCheck$Taxable.Income)

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)

barplot(table(FC$Risky_Good))
table(FC$Risky_Good)

FC_train <- FC[1:300,]
FC_test <- FC[301:600,]

###Using Party Function 

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)

plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky and 80 % good 

# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)

CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df) # 82 % Accuracy

# balancing the data
balance <- as.integer(sample(rownames(FC[Risky_Good=="Good",]),124,replace = F))
FC_1 <- rbind(FC[Risky_Good=="Risky",],FC[balance,])
barplot(table(FC_1$Risky_Good))

set.seed(101)
intraininglocal <- as.integer(sample(x = nrow(FC_1),size = nrow(FC_1)*.7,F))
length(intraininglocal)
train_F <- FC_1[intraininglocal,]; 
test_F <- FC_1[-intraininglocal,];

Tree <- C5.0(train_F$Risky_Good~.,data = train_F[,-c(7,3,5)],trials=50)
summary(Tree)
predf <- predict(Tree,test_F[,-c(7,3,5)])

table(Actual =test_F$Risky_Good ,Predicted = predf)
mean(test_F$Risky_Good==predf) 
