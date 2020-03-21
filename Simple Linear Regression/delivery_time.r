Delivery_Time <- read.csv(file.choose()) # choose the data set
View(Delivery_Time)

summary(Delivery_Time)

var(Delivery_Time$Delivery.Time)
var(Delivery_Time$Sorting.Time)

sd(Delivery_Time$Sorting.Time)
sd(Delivery_Time$Delivery.Time)

library(moments)
#Skewness and Kurtosis
skewness(Delivery_Time$Delivery.Time)
kurtosis(Delivery_Time$Delivery.Time)

skewness(Delivery_Time$Sorting.Time)
kurtosis(Delivery_Time$Sorting.Time)

############## Visualization ###############

plot(Delivery_Time$Sorting.Time,Delivery_Time$Delivery.Time) # Scatter Plot

barplot(Delivery_Time$Delivery.Time,col=rainbow(8))
boxplot(Delivery_Time$Delivery.Time,horizontal = T)
hist(Delivery_Time$Delivery.Time,col=rainbow(8))
qqnorm(Delivery_Time$Delivery.Time)
qqline(Delivery_Time$Delivery.Time)

barplot(Delivery_Time$Sorting.Time,col=rainbow(8))
boxplot(Delivery_Time$Sorting.Time,horizontal = T ) #Based on Boxplot we dont have outliears
hist(Delivery_Time$Sorting.Time,col=rainbow(8))
qqnorm(Delivery_Time$Sorting.Time) # Based on qqnorm we confirmed that data is linearly Distributed
qqline(Delivery_Time$Sorting.Time)

############## Correlation coefficient ###############
# Correlation coefficient value for Delivery Time and Sorting Time
dt<- Delivery_Time$Delivery.Time
st <- Delivery_Time$Sorting.Time
cor(st,dt)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.8259973).

############## Simple Linear Regression model ##########
# Simple model without using any transformation
reg<-lm(dt~st)
summary(reg)

# Probability value should be less than 0.05(0.00115)
# The multiple-R-Squared Value is 0.6823 which is lesser than 0.8(In General)
# Adjusted R-Squared Value is 0.6655 

confint(reg,level = 0.95) # confidence interval

# Function to Predict the above model 

predict(reg,interval="predict")

# Adjusted R-squared value for the above model is 0.6655 
# we may have to do transformation of variables for better R-squared value

################ Logarthmic model ###############
reg_log<-lm(dt~log(st))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)

predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.6954
# Adjusted R-squared:  0.6794 

############ Exponential model ####################
reg_exp<-lm(log(dt)~st) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)

exp(predict(reg_exp,interval="predict"))

# R-squared value - 0.7109
# Adjusted R SQuare Value - 0.6957 

######### Quadratic model #####################
Delivery_Time[,"st_sq"] = st*st

# Quadratic model
quad_mod <- lm(dt~st+I(st^2),data=Delivery_Time)
summary(quad_mod)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.6594
#Multiple R -Squared Value = 0.6934

# Quadratic model
qd_model <- lm(dt~st+st_sq,data=Delivery_Time)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.6594
#Multiple R -Squared Value = 0.6934

####### Polynomial model with 3 degree #############
poly_mod <- lm(dt~st+I(st^2)+I(st^3),data=Delivery_Time)
summary(poly_mod) 

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")

# Adjusted R-Squared = 0.6511
#Multiple R -Squared Value = 0.7034

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.6655,0.6794,0.6957,0.6594,0.6511)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Exponential  model gives the best Adjusted R-Squared value
predicted_Value <- exp(predict(reg_exp))
predicted_Value

Final <- cbind(Sorting_Time=Delivery_Time$Sorting.Time ,Delivery_Time = Delivery_Time$Delivery.Time,Predicted_Delivery_time=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-dt)^2))
rmse

plot(reg_exp)
hist(residuals(reg_exp)) # close to normal distribution
