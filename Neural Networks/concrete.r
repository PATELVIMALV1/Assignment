# Libraries Used
install.packages("ggvis")
library(ggvis) #Data visulization
install.packages("psych")
library(psych) #Scatterplot matrix
library(knitr) #html table
install.packages("neuralnet")
library(neuralnet) #artifical neural network 

# Objective
#This project aims to develop a reliable model using Artificial Neural Networks (ANN) 
#to predict concrete strength given a list of composition inputs.

#Data Exploration
#The concrete dataset contains 1,030 examples of concrete with 
#eight features describing the components used in the mixture.

#Data Preview
concrete <- read.csv(file.choose())
View(concrete)
head(concrete)
# Data Structure
str(concrete)
summary(concrete)

# Data Visualization
# Histogram
hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))

hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))

hist(concrete$water, prob = T, breaks = 30)
lines(density(concrete$water))

hist(concrete$superplastic, prob = T, breaks = 30)
lines(density(concrete$superplastic))

hist(concrete$coarseagg, prob = T, breaks = 30)
lines(density(concrete$coarseagg))

hist(concrete$fineagg, prob = T, breaks = 30)
lines(density(concrete$fineagg))

hist(concrete$age, prob = T, breaks = 30)
lines(density(concrete$age))

pairs.panels(concrete[c("cement", "slag", "water","superplastic","coarseagg","fineagg","age","ash", "strength")])

#As most of the feature's and target variable are not normally distributed as
#seen from the scatterplot matrices, normalization needs to be done
# Normalization
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")

summary(concrete_norm)

# Data Partition
#After normalization, the dataset is ready to be split into 
#its training set and test set. The proportion used here will be 75% training and 25% test.

nrow(concrete_norm)
#training set
concrete_train <- concrete_norm[1:773, ]

#test set
concrete_test <- concrete_norm[774:1030, ]

#Build a neural network with one hidden layer 
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + 
                              fineagg + age , data = concrete_train, hidden = 1)

plot(concrete_model)

#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength) 

#building the new model with hidden layer = 5
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + 
                               fineagg + age, data = concrete_train, hidden = 5 )
plot(concrete_model2)

#Building the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])

#storing the results
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
