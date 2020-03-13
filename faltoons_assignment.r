faltoons<-read.csv("D:/Assignment/Hypothesis testing/Faltoons.csv")
View(faltoons)
summary(faltoons)
#Weekdays     Weekend   
#Female:287   Female:233  
#Male  :113   Male  :167  

#Ho= Proportions of Male and Female are same
#Ha= Proportions of Male and Female are not same

prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided") # weekdays
prop.test(x=c(287,233),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided") # weekend

#P-value is less than 0.05 and hence we reject Null.Hence proportions of Male and Female are not same

#Ho= Proportions of Male is less than or equal to Female
#Ha= Proportions of Male is greater than Female

prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "less") 

#P-value <0.05 and hence we reject null. Hence proportion of Male is greater than Female.


