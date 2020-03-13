cof<-read.csv("D:/Assignment/Hypothesis testing/Costomer+OrderForm.csv")
View(cof)
summary(cof)
#Phillippines      Indonesia          Malta            India    
#Defective : 29   Defective : 33   Defective : 31   Defective : 20  
#Error Free:271   Error Free:267   Error Free:269   Error Free:280  

#Ho= Proportions of Defective and Error Free are same
#Ha= Proportions of Defective and Error Free are not same

x<-matrix(c(29,271,33,267,31,269,20,280), nrow = 2)
View(x)
#P-value > 0.05 and hence we fail to reject Null.
#Hence proportions of Defective and Error Free same

