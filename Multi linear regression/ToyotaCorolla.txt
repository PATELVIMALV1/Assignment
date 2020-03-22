Toyota  <- read.csv(file.choose()) # choose the data set
Corolla<-Toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
summary(Corolla)
########## Standard Deviation #########
sd(Corolla$Price)
sd(Corolla$Age_08_04)
sd(Corolla$KM)
sd(Corolla$HP)
sd(Corolla$cc)
sd(Corolla$Doors)
sd(Corolla$Gears)
sd(Corolla$Quarterly_Tax)
sd(Corolla$Weight)

######### Variance ###########
var(Corolla$Price)
var(Corolla$Age_08_04)
var(Corolla$KM)
var(Corolla$HP)
var(Corolla$cc)
var(Corolla$Doors)
var(Corolla$Gears)
var(Corolla$Quarterly_Tax)
var(Corolla$Weight)

####### Correlation Coefficient ###########
windows()
pairs(Corolla)
cor(Corolla)

###### Pure Correlation  b/n the varibles #######
cor2pcor(cor(Corolla))

##### Linear regression model #############
model <- lm(Price ~ ., data = Corolla)
summary(model)

# cc and Doors are influence to each other
#predict the model based on individual records
model.carcc <- lm(Price ~ cc,data = Corolla)
summary(model.carcc) # Its significat 

model.cardoor <- lm(Price ~ Doors,data = Corolla)
summary(model.cardoor) # It's  significatnt

###### Build model with cc and Doors ##########
model.car <- lm(Price ~ cc + Doors,data = Corolla)
summary(model.car) # Both are significant to each other

######## Identifying influential variable #########
influence.measures(model.car)
influenceIndexPlot(model.car)
influencePlot(model.car)

# Delete influentails records and build the model
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)
vif(model1)
avPlots(model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel)

plot(finalmodel)

qqPlot(finalmodel)

stepAIC(finalmodel)
