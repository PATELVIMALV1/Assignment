emp <- read.csv(file.choose()) # choose the data set
View(emp)
summary(emp)

var(emp$Salary_hike)
var(emp$Churn_out_rate)

sd(emp$Salary_hike)
sd(emp$Churn_out_rate)

library(moments)
#Skewness and Kurtosis
skewness(emp$Salary_hike)
kurtosis(Delivery_Time$Delivery.Time)

skewness(emp$Salary_hike)
kurtosis(emp$Churn_out_rate)

############## Visualization ###############
plot(emp) # Scatter Plot

barplot(emp$Salary_hike,col=rainbow(8))
boxplot(emp$Salary_hike,horizontal = T)
hist(emp$Salary_hike,col=rainbow(8))
qqnorm(emp$Salary_hike)
qqline(emp$Salary_hike)

barplot(emp$Churn_out_rate,col=rainbow(8))
boxplot(emp$Churn_out_rate,horizontal = T ) #Based on Boxplot we dont have outliears
hist(emp$Churn_out_rate,col=rainbow(8))
qqnorm(emp$Churn_out_rate) # Based on qqnorm we confirmed that data is linearly Distributed
qqline(emp$Churn_out_rate)

############## Correlation coefficient ###############
# Correlation coefficient value for Salary_hike and Churn_out_rate
sh<- emp$Salary_hike
cr <- emp$Churn_out_rate
cor(sh,cr)

############## Simple Linear Regression model ##########
# Simple model without using any transformation
reg<-lm(cr~sh)
summary(reg)

# Probability value should be less than 0.05(1.96e-05)
# The multiple-R-Squared Value is 0.8312 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.8101 

confint(reg,level = 0.95) # confidence interval

# Function to Predict the above model 

predict(reg,interval="predict")

# Adjusted R-squared value for the above model is 0.8101 
# we may have to do transformation of variables for better R-squared value

################ Logarthmic model ###############
reg_log<-lm(cr~log(sh))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)

predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.8486
# Adjusted R-squared:  0.8297

############ Exponential model ####################
reg_exp<-lm(log(cr)~sh) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)

exp(predict(reg_exp,interval="predict"))

# R-squared value - 0.8577
# Adjusted R SQuare Value - 0.8735

######### Quadratic model #####################
emp[,"sh_sq"] = sh*sh

# Quadratic model
quad_mod <- lm(cr~sh+I(sh^2),data=emp)
summary(quad_mod)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9662
#Multiple R -Squared Value = 0.9737

# Quadratic model
qd_model <- lm(cr~sh+sh_sq,data=emp)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9662
#Multiple R -Squared Value = 0.9737

####### Polynomial model with 3 degree #############
poly_mod <- lm(cr~sh+I(sh^2)+I(sh^3),data=emp)
summary(poly_mod) # 0.9811

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")

# Adjusted R-Squared = 0.984
#Multiple R -Squared Value = 0.9893

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8101,0.8297,0.8735,0.9662,0.984)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Polynomial model with 3 degree gives the best Adjusted R-Squared value
pred_final <- predict(poly_mod)
pred_final

rmse<-sqrt(mean((pred_final-cr)^2))
rmse

plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution

