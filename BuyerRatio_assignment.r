Buy<-read.csv("D:/Assignment/Hypothesis testing/BuyerRatio.csv")
View(Buy)
summary(Buy)

#Ho= Proportions of Male and Female are same
#Ha= Proportions of Male and Female are not same

x<-matrix(c(50,435,142,1523,131,1356,70,750), nrow = 2)
View(x)
chisq.test(x)

#P-value>0.05.Hence we fail to reject Null.
#Hence proportion of male and female across regions is same.