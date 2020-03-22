startup_50  <- read.csv(file.choose()) # choose the data set
View(startup_50 )

summary(startup_50)

######### Variance ###########
var(startup_50$R.D.Spend)
var(startup_50$Administration)
var(startup_50$`Marketing.Spend`)
var(startup_50$Profit)

########## Standard Deviation #########
sd(startup_50$R.D.Spend)
sd(startup_50$Administration)
var(startup_50$`Marketing.Spend`)
sd(startup_50$Profit)

# correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
windows()
pairs(startup_50)

########## Correlation coefficient ##########
startup_50[4]<-NULL
View(startup_50)
cor(startup_50)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup_50))

# The Linear Model of interest with all the columns
model <- lm(startup_50$Profit~.,data=startup_50)
summary(model)

# Multicollinearity check
# Model based on only Administration
model.adm<-lm(startup_50$Profit~startup_50$Administration,data=startup_50)
summary(model.adm) # Administration is insignificant

# Model based on only Weight
model2.ms<-lm(startup_50$Profit~startup_50$Marketing.Spend,data=startup_50)
summary(model2.ms) # Marketing.Spend became significant

# Model based on Administration and Marketing.Spend
model3.adm_ms<-lm(startup_50$Profit~startup_50$Administration+startup_50$Marketing.Spend,data=startup_50)
summary(model3.adm_ms) # Both became significant

# Applying VIF function on model built on all inputs
library(car)
vif(model) # Original model
vif(model3.adm_ms)

## Added Variable plot to check correlation b/n variables and o/p variable
windows()
avPlots(model,id.n=2,id.cex=0.7)

# Deletion Diagnostics for identifying influential variable
influence.measures(model)

influenceIndexPlot(model, id.n=3) # Index Plots of the influence measures
influencePlot(model, id.n=3)
startup_50<-startup_50[-c(49,50),]
## Regression after deleting the 49th and 50th observation, which is influential observation

Model.reg<-lm(startup_50$Profit~.,data=startup_50) 
summary(Model.reg) #Adjusted R2 Value = 0.9601  
confint(Model.reg,level=0.95)
predict(Model.reg,interval="predict")
#Adjusted R2 Value = 0.9601

pr<-startup_50$Profit
rd<-startup_50$R.D.Spend
adm<-startup_50$Administration
ms<-startup_50$Marketing.Spend

######### Logarthmic transformation ############
Model.reg_log_adm<-lm(pr~rd+log(adm)+ms,data=startup_50)
summary(Model.reg_log_adm) 
confint(Model.reg_log_adm,level=0.95)
predict(Model.reg_log_adm,interval="predict")
#Adjusted R2 Value = 0.9599 

######## Exponential model ##############
Model.reg_exp<-lm(log(pr)~rd+adm+ms,data=startup_50)
summary(Model.reg_exp) 
confint(Model.reg_exp,level=0.95)
predict(Model.reg_exp,interval="predict")
#Adjusted R2 Value = 0.9198

############ Quadratic model ##########
Model.reg_quad<-lm(pr~rd+I(rd^2)+adm+I(adm^2)+ms+I(ms^2),data=startup_50)
summary(Model.reg_quad) 
confint(Model.reg_quad,level=0.95)
predict(Model.reg_quad,interval="predict")
#Adjusted R2 Value = 0.9587

########## Polynomial model with 3 degree ###############
Model.reg_poly<-lm(pr~rd+I(rd^2)+I(rd^3)+adm+I(adm^2)+I(adm^3)+ms+I(ms^2)+I(ms^3),data=startup_50)
summary(Model.reg_poly) 
confint(Model.reg_poly,level=0.95)
predict(Model.reg_poly,interval="predict")
#Adjusted R2 Value = 0.9591

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("Model.reg","Model.reg_log_adm","Model.reg_exp","Model.reg_quad","Model.reg_poly")
model_R_Squared_values[["R_squared"]] <- c(0.9601,0.9599 ,0.9198,0.9587,0.9591)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(Final)

# Model.reg gives the best Adjusted R-Squared value
pred_final <- predict(Model.reg)
pred_final

rmse<-sqrt(mean((pred_final-pr)^2))
rmse

plot(Model.reg)

hist(residuals(Model.reg)) # close to normal distribution

qqPlot(Model.reg, id.n=5)
library(MASS)
stepAIC(Model.reg) # backward
# Lower the AIC value better is the model

