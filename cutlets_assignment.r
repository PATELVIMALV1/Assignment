cutlets<-read.csv("D:/Assignment/Hypothesis testing/Cutlets.csv")
View(cutlets)
summary(cutlets)
#Unit.A          Unit.B
#Mean   :7.019   Mean   :6.964

#Normality test
#Ho= Data is normally distributed
#Ha=Data is not normally distributed

shapiro.test(cutlets$Unit.A)

# p-value = 0.32 >0.05
#Fail to reject null hypothesis
#can assume data is normally distributed for Unit.A

shapiro.test(cutlets$Unit.B)
# p-value = 0.52 >0.05
#Fail to reject null hypothesis
#can assume data is normally distributed for Unit.A

#Variance test
#Ho= Variance of diameters of Unit A is equal to the variance of diameters of Unit B
#Ha= Variance of diameters of Unit A is not equal to the variance of diameters of Unit B
var.test(cutlets$Unit.A,cutlets$Unit.B)
#p-value = 0.31 > 0.05
#Fail to reject null hypothesis
#Variance of diameters of Unit A is equal to the variance of diameters of Unit B


#2 sample T Test 
#Ho= Averages of diameters of Unit A is equal to Averages of diameters of unit B
#Ha= Averages of diameters of Unit A is not equal to Averages of diameters of unit B
t.test(cutlets$Unit.A,cutlets$Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)

#P-value = 0.47 > 0.05 
#Fail to reject null hypothesis
#Averages of diameters of Unit A is equal to Averages of diameters of unit B
#Inference is that there is no significant difference in the diameters of Unit A and Unit B
#95 percent confidence interval:
# -0.09654633  0.20613490
#Visual represntation
boxplot(cutlets, main = 'Cutlets')
