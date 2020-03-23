library(randomForest)
library(MASS)
library(caret)
# USe the set.seed function so that we get same results each time 
set.seed(123)
data(iris)
View(iris)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

rf <- randomForest(Species~., data=iris_train)
rf
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, iris_train)
head(pred1)
head(iris_train$Species)
# looks like the first six predicted value and original value matches.
confusionMatrix(pred1, iris_train$Species)  # 100 % accuracy on training data 
# Sensitivity for all three species/categories is 100 % 
# Prediction with test data - Test Data 
pred2 <- predict(rf, iris_test)
confusionMatrix(pred2, iris_test$Species) # 94.67% accuracy on test data
# Error Rate in Random Forest Model
plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(iris_train[,-5], iris_train[,5], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Species~., data=iris_train, ntree = 140, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1  # with the new values after tuning, the OOB estimate error is 4 %

pred1 <- predict(rf1, iris_train)
confusionMatrix(pred1, iris_train$Species)  # 100 % accuracy on training data 
# Sensitivity for Yes and No is 100 % 
# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, iris_test)
confusionMatrix(pred2, iris_test$Species) # 96 % accuracy on test data 
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Majority of the trees has an average number has close to 40 nodes.
# Variable Importance
varImpPlot(rf1)

varImpPlot(rf1 ,Sort = T, n.var = 4, main = "Top 4 -Variable Importance")

importance(rf1)
# Partial Dependence Plot 
partialPlot(rf1, iris_train, Petal.Length, "versicolor")

partialPlot(rf1, iris_train, Petal.Length, "setosa")

partialPlot(rf1, iris_train, Petal.Length, "virginica")

# if the petal.length is between 2.5 to 5,5, then it is Versicolor
# If the petal.length is between 1 to 3 cms in length, then it is setosa
# if the petal.length is greater than 3 cms in lenth, then it is Virginica

# Extract single tree from the forest
tr1 <- getTree(rf1, 1, labelVar = TRUE)
tr1
# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, iris$Species)
