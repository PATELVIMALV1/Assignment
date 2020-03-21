Salary_Data <- read.csv(file.choose()) # choose the data set
View(Salary_Data)

summary(Salary_Data)

var(Salary_Data$YearsExperience)
var(Salary_Data$Salary)

sd(Salary_Data$YearsExperience)
sd(Salary_Data$Salary)

library(moments)
#Skewness and Kurtosis
skewness(Salary_Data$YearsExperience)
kurtosis(Salary_Data$YearsExperience)

skewness(Salary_Data$Salary)
kurtosis(Salary_Data$Salary)

############## Visualization ###############

plot(Salary_Data$Salary,Salary_Data$YearsExperience) # Scatter Plot

barplot(Salary_Data$YearsExperience,col=rainbow(8))
boxplot(Salary_Data$YearsExperience,horizontal = T)
hist(Salary_Data$YearsExperience,col=rainbow(8))
qqnorm(Salary_Data$YearsExperience)
qqline(Salary_Data$YearsExperience)

barplot(Salary_Data$Salary,col=rainbow(8))
boxplot(Salary_Data$Salary,horizontal = T ) #Based on Boxplot we dont have outliears
hist(Salary_Data$Salary,col=rainbow(8))
qqnorm(Salary_Data$Salary) # Based on qqnorm we confirmed that data is linearly Distributed
qqline(Salary_Data$Salary)

############## Correlation coefficient ###############
# Correlation coefficient value for Delivery Time and Sorting Time
ye<- Salary_Data$YearsExperience
sl <- Salary_Data$Salary
cor(sl,ye)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.9782416).

############## Simple Linear Regression model ##########
# Simple model without using any transformation
reg<-lm(sl~ye)
summary(reg)

# Probability value should be less than 0.05(5.51e-12)

confint(reg,level = 0.95) # confidence interval

# Function to Predict the above model 

predict(reg,interval="predict")

# Adjusted R-squared value for the above model is 0.9554 
# we may have to do transformation of variables for better R-squared value

################ Logarthmic model ###############
reg_log<-lm(sl~log(ye))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)

predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.8539
# Adjusted R-squared:  0.8487

############ Exponential model ####################
reg_exp<-lm(log(sl)~ye) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)

exp(predict(reg_exp,interval="predict"))

# R-squared value - 0.9295
# Adjusted R SQuare Value - 0.932 

######### Quadratic model #####################
Salary_Data[,"ye_sq"] = ye*ye

# Quadratic model
quad_mod <- lm(sl~ye+I(ye^2),data=Salary_Data)
summary(quad_mod)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9538 
#Multiple R -Squared Value = 0.957

# Quadratic model
qd_model <- lm(sl~ye+ye_sq,data=Salary_Data)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9538
#Multiple R -Squared Value = 0.957

####### Polynomial model with 3 degree #############
poly_mod <- lm(sl~ye+I(ye^2)+I(ye^3),data=Salary_Data)
summary(poly_mod) 

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")

# Adjusted R-Squared = 0.9594
#Multiple R -Squared Value = 0.9636

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.9554,0.8487,0.932,0.9538,0.9594)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Polynomial model with 3 degree gives the best Adjusted R-Squared value
pred_final <- predict(poly_mod)
pred_final

rmse<-sqrt(mean((pred_final-sl)^2))
rmse

plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution
