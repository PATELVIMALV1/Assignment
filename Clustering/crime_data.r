crime_data <- read.csv(file.choose())
View(crime_data)
summary(crime_data)

crime_data1 <- crime_data[2:5]
boxplot(crime_data1[,"Murder"])
boxplot(crime_data1[,"Assault"])
boxplot(crime_data1[,"Rape"])
boxplot(crime_data1[,"UrbanPop"])

# All the data is numeric but scales are very different. hence need to scale the data
crime_data_scaled <- scale(crime_data1)
crime_data_scaled

#############Hierarchical clustering ###################

# Calculate Euclidean distance and then run Hierarchical clustering
d <- dist(crime_data_scaled, method = "euclidean")
str(d)

# Clusters using average linkage method
fit_Avg <- hclust(d,method="average")
windows()
plot(fit_Avg) 

# Looking at the height, there seem to be 4 logical clusters  + 1 outlier
groups_Avg <- cutree(fit_Avg,k=5)
Membership_Avg <- as.matrix(groups_Avg)
Membership_Avg
rect.hclust(fit_Avg,k=5,border="red")

# Clusters using single linkage method
fit_Sngl <- hclust(d,method="single")
plot(fit_Sngl) 
# This doesn't give clear clusters apart from the outlier
# Clusters using complete linkage method
fit_Compl <- hclust(d,method="complete")
plot(fit_Compl) 
# Based on dendogram, this gives 4 clear clusters
groups_Compl <- cutree(fit_Compl,k=4)
Membership_Compl <- as.matrix(groups_Compl)
Membership_Compl
rect.hclust(fit_Compl,k=4,border="red")
# draw dendogram with red borders around all clusters

Clusters <- data.frame(crime_data,groups_Avg,groups_Compl)
Clusters

############# K-means clustering ###########

install.packages("plyr")
library(plyr)
install.packages("animation")
library(animation)

km4 <- kmeans(crime_data_scaled,4)

#Scree plot
wss <- c()
for (i in 2:15)
  wss[i] <- sum(kmeans(crime_data_scaled,centers=i)$withinss)
plot(1:15,wss,type="b",xlab="K",ylab="Avg Distance-within cluster")

# based on the scree plot, k=4 clusters seem to be the hocky stick point. 
# Beyond this the value add is very low. The next point could be at k=8

km8 <- kmeans(crime_data_scaled,8)
km8$cluster

Clusters_KM4 <- data.frame(crime_data,km4$cluster)
Clusters_KM4

Clusters_km8 <- data.frame(crime_data,km8$cluster)
Clusters_km8

cl<- kmeans.ani(crime_data_scaled, 4)
#It can be seen that the data is divided into 4 clusters. The cluster centers are :
cl$centers
