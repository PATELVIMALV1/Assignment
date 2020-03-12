TAT<-read.csv("D:/Assignment/Hypothesis testing/LabTAT.csv")
View(TAT)
summary(TAT)
#Laboratory.1    Laboratory.2    Laboratory.3    Laboratory.4  
#Mean   :178.4   Mean   :178.9   Mean   :199.9   Mean   :163.7 

#Normality test
#Ho= Data is normally distributed
#Ha=Data is not normally distributed

shapiro.test(TAT$Laboratory.1) # p-value = 0.5508
shapiro.test(TAT$Laboratory.2) # p-value = 0.8637
shapiro.test(TAT$Laboratory.3) # p-value = 0.4205
shapiro.test(TAT$Laboratory.4) # p-value = 0.6619

# For all feature  p-value >0.05
#Fail to reject null hypothesis
#can assume  all data is normally distributed for TAT

#Variance test
#Ho= Variance of TAT of Lab is equal 
#Ha= Variance of TAT of Lab is not equal 

var.test(TAT$Laboratory.1,TAT$Laboratory.2) # p-value = 0.1675
var.test(TAT$Laboratory.2,TAT$Laboratory.3) # p-value = 0.2742
var.test(TAT$Laboratory.3,TAT$Laboratory.4) # p-value = 0.3168
var.test(TAT$Laboratory.4,TAT$Laboratory.1) # p-value = 0.1408

# For all feature  p-value >0.05
#Fail to reject null hypothesis
#Variance of TAT of Lab is equal 

#As there are more than 2 discrete variables and output variable TAT is a continuous variable. Hence we will go with Anova one way test.
#Ho= Average TAT for all the samples is same
#Ha= Averages TAT for all the samples is not same
results<-aov(TAT$Laboratory.1~TAT$Laboratory.2,data=TAT)
summary(results) #  p = 0.168
results1<-aov(TAT$Laboratory.2~TAT$Laboratory.3,data=TAT)
summary(results1) # p = 0.474
results2<-aov(TAT$Laboratory.3~TAT$Laboratory.4,data=TAT)
summary(results2) # p = 0.173
results3<-aov(TAT$Laboratory.4~TAT$Laboratory.1,data=TAT)
summary(results3) # p = 0.315

#For different Lab combination we have p value > 0.05
#Fail to reject null hypothesis
#Average TAT for all the samples is same

#Hence there is no significant difference in the average TAT for all the labs.
# Visualization
boxplot(TAT, main = 'LabTAT')



