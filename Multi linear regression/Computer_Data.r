ComputerData  <- read.csv('D:/Assignment/Multi linear regression/Computer_Data.csv')
ComputerData  <- read.csv(file.choose()) # choose the data set
View(ComputerData)
ComputerData <- ComputerData[,-1]
summary(ComputerData)
########## Standard Deviation #########
sd(ComputerData$price)
sd(ComputerData$speed)
sd(ComputerData$hd)
sd(ComputerData$ram)
sd(ComputerData$screen)
sd(ComputerData$ads)
sd(ComputerData$trend)
######### Variance ###########
var(ComputerData$price)
var(ComputerData$speed)
var(ComputerData$hd)
var(ComputerData$ram)
var(ComputerData$screen)
var(ComputerData$ads)
var(ComputerData$trend)

ComputerData<-data.frame(ComputerData$price,ComputerData$speed,ComputerData$hd,
                         ComputerData$ram,ComputerData$screen,ifelse(ComputerData$cd=='yes',1,0),
                         ifelse(ComputerData$multi=='yes',1,0),ifelse(ComputerData$premium=='yes',1,0),
                         ComputerData$ads,ComputerData$trend)
colnames(ComputerData)
ComputerData<-data.frame("price"=ComputerData$ComputerData.price,"speed"= ComputerData$ComputerData.speed,
                         "hd"=ComputerData$ComputerData.hd,"ram"=ComputerData$ComputerData.ram,
           "screen"=ComputerData$ComputerData.screen,"cd"=ComputerData$ifelse.ComputerData.cd.....yes...1..0.,
           "multi"=ComputerData$ifelse.ComputerData.multi.....yes...1..0.,
           "premium"=ComputerData$ifelse.ComputerData.premium.....yes...1..0.,
           "ads"= ComputerData$ComputerData.ads,"trend"= ComputerData$ComputerData.trend)
                    
####### Correlation Coefficient ###########
windows()
pairs(ComputerData)
cor(ComputerData)

###### Pure Correlation  b/n the varibles #######
cor2pcor(cor(Corolla))
# The Linear Model of interest with all the columns
model <- lm(price ~., data = ComputerData)
summary(model)

# Applying VIF function on model built on all inputs
library(car)
vif(model) # Original model
## Added Variable plot to check correlation b/n variables and o/p variable
windows()
avPlots(model,id.n=2,id.cex=0.7)

# Deletion Diagnostics for identifying influential variable
influence.measures(model)

influenceIndexPlot(model, id.n=3) # Index Plots of the influence measures
influencePlot(model, id.n=3)
ComputerData<-ComputerData[-c(1441,1701),]
## Regression after deleting the 1441th and 1701th observation, which is influential observation

Model.reg<-lm(price ~., data = ComputerData) 
summary(Model.reg) #Adjusted R2 Value = 0.7774  
confint(Model.reg,level=0.95)
predict(Model.reg,interval="predict")

######## Exponential model ##############
Model.reg_exp<-lm(log(price)~.,data=ComputerData)
summary(Model.reg_exp) 
confint(Model.reg_exp,level=0.95)
predict(Model.reg_exp,interval="predict")
#Adjusted R2 Value = 0.7833
colnames(ComputerData)
############ Quadratic model ##########
Model.reg_quad<-lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+
                     screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+
                   premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data=ComputerData)
summary(Model.reg_quad) 
confint(Model.reg_quad,level=0.95)
predict(Model.reg_quad,interval="predict")
#Adjusted R2 Value = 0.8049

########## Polynomial model with 3 degree ###############
Model.reg_poly<-lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+
                     screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+
                     premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data=ComputerData)
summary(Model.reg_poly) 
confint(Model.reg_poly,level=0.95)
predict(Model.reg_poly,interval="predict")
#Adjusted R2 Value = 0.813

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("Model.reg","Model.reg_exp","Model.reg_quad","Model.reg_poly")
model_R_Squared_values[["R_squared"]] <- c(0.7774 ,0.7833,8049,0.813)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(Final)

# Model.reg_poly gives the best Adjusted R-Squared value
pred_final <- predict(Model.reg_poly)
pred_final
pr<-ComputerData$price
rmse<-(sqrt(mean((pred_final-pr)^2)))
rmse

plot(Model.reg_poly)

hist(residuals(Model.reg_poly)) # close to normal distribution

qqPlot(Model.reg_poly, id.n=5)
library(MASS)
stepAIC(Model.reg_poly) # backward
# Lower the AIC value better is the model
