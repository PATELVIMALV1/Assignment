library(readxl)
EastWestAirlines <- read_excel(file.choose(),sheet = "data")
View(EastWestAirlines)
summary(EastWestAirlines)
# variables cc1, cc2, cc3, award are dummy variables in the range of 1 to 5, Since this range is similar to z-score it doen't need scaling
# Scaling the variables excluding the dummy variables

Airline_num_data_scaled <- scale(EastWestAirlines[c(2,3,7,8,9,10,11)])
Airline_num_data_scaled
Airlines_Data_Forclustering <- cbind(Airline_num_data_scaled,EastWestAirlines[,c(4,5,6,12)])
Airlines_Data_Forclustering

#############Hierarchical clustering ###################

# Calculate Euclidean distance and then run Hierarchical clustering
d <- dist(Airlines_Data_Forclustering, method = "euclidean")

# Clusters using average linkage method
fit_Avg <- hclust(d,method="average")
windows()
plot(fit_Avg) 

rect.hclust(fit_Avg,k=7,border="red")

# Clusters using Complete linkage method
fit_Compl <- hclust(d,method="complete")
windows()
plot(fit_Compl) 
rect.hclust(fit_Compl,k=6,border="red")
rect.hclust(fit_Compl,k=4,border="red")

# Clusters using single linkage method
fit_Sngl <- hclust(d,method="single")
plot(fit_Sngl) 
############# K-means clustering ###########

install.packages("plyr")
library(plyr)


km4 <- kmeans(Airlines_Data_Forclustering,5)
windows()

#Scree plot
wss <- c()
for (i in 2:15)
  wss[i] <- sum(kmeans(Airlines_Data_Forclustering,centers=i)$withinss)
plot(1:15,wss,type="b",xlab="K",ylab="Avg Distance-within cluster")

# based on scree plot, let's try 9 clusters
km9 <- kmeans(Airlines_Data_Forclustering,9)
Clusters_KM9 <- data.frame(Airlines_Data_Forclustering,km9$cluster)
Clusters_KM9
library(animation)
cl<- kmeans.ani(Airlines_Data_Forclustering, 9)

write.csv(Clusters_KM9,file="filnal.csv")
