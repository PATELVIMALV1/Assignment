# Libraries Used
install.packages("ggvis")
library(ggvis) #Data visulization
install.packages("psych")
library(psych) #Scatterplot matrix
library(knitr) #html table
install.packages("neuralnet")
library(neuralnet)
library(plyr)

#Data Preview
Startups <- read.csv(file.choose())
View(Startups)
head(Startups)
# Data Structure
str(Startups)
summary(Startups)

Startups <- as.factor(Startups)

colnames(Startups)

hist(Startups$R.D.Spend, prob = T, breaks = 20)
lines(density(Startups$R.D.Spend))

hist(Startups$Administration, prob = T, breaks =20)
lines(density(Startups$Administration))

hist(Startups$Marketing.Spend, prob = T, breaks = 20)
lines(density(Startups$Marketing.Spend))

pairs.panels(Startups[c("R.D.Spend", "Administration", "Marketing.Spend","State","Profit")])

Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
cor(Startups)
class(Startups)

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit) # Normalized form of profit
summary(Startups$profit)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]

# Creating a neural network model on training data

startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)

plot(startups_model, rep = "best")

# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")

model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)
