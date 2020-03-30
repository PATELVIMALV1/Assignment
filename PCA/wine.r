wine_data <- read.csv(file.choose())
View(wine_data)

## the first column in wine_data has Types of Wine
View(wine_data[-1]) 
# wine_data[-1] -> Considering only numerical values for applying PCA
data <- wine_data[,-1]
attach(data)
cor(data)

# cor = TRUE use correlation matrix for getting PCA scores
?princomp
wine_pca<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
summary(wine_pca)
View(wine_pca$scores)
str(wine_pca)

#Visualize using 1st 2 Principle components
windows()
plot(wine_pca$scores,col="Blue",pch=18, cex =0.3, lwd = 3)
text(wine_pca$scores, labels = c(1:178), cex=1)

plot(wine_pca) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)
biplot(wine_pca)

############# Hierarchical Clustering#######################

# Top 3 PCA Scores which represents the whole data
# The range os values of PCs are slightly different hence scaling
wine_pca_scaled <- scale(wine_pca$scores[,1:3])
View(wine_pca_scaled)
# Hierarchical clustering
d <- dist(wine_pca_scaled, method = "euclidean")

# Clusters using average linkage method
fit_Avg <- hclust(d,method="average")
windows()
plot(fit_Avg) 
rect.hclust(fit_Avg,k=3,border="red")

# Clusters using complete linkage method
fit_Com <- hclust(d,method="complete")
windows()
plot(fit_Com) 
rect.hclust(fit_Com,k=3,border="red")

#Try clustering without scaling

d2 <- dist(wine_pca$scores[,1:3], method = "euclidean")
# Clusters using average linkage method
fit_Avg <- hclust(d2,method="average")
windows()
plot(fit_Avg) 
rect.hclust(fit_Avg,k=3,border="red")

# Clusters using complete linkage method
fit_Com <- hclust(d2,method="complete")
windows()
plot(fit_Com) 
rect.hclust(fit_Com,k=3,border="red")

# Clusters using single linkage method
fit_sin <- hclust(d2,method="single")
windows()
plot(fit_sin) 

##Hierarchical clustering without scaling of 3 PCs with complete linkage give clearer clusters

############ k-mean clustering ################

windows()

#Scree plot
wss <- c()
for (i in 2:15)
  wss[i] <- sum(kmeans(wine_pca$scores[,1:3],centers=i)$withinss)
plot(1:15,wss,type="b",xlab="K",ylab="Avg Distance-within cluster")


km3 <- kmeans(wine_pca$scores[,1:3],3)
Clusters_km3 <- data.frame(wine_data,km3$cluster)
View(Clusters_km3)

# the cluster numbers 2 & 3 need to be swapped
for (i in 1:178) {
  if(Clusters_km3[i,15] == 2) Clusters_km3[i,15] <- 3
  else if(Clusters_km3[i,15] == 3) Clusters_km3[i,15] <- 2
}
View(Clusters_km3)

############### Verification & Conclusion ######################
# Compare and findout # differences between class given in the data and clusters formed thru PCA
Comparison_table <-table(Clusters_km3[,1],Clusters_km3[,15])
Comparison_table
# accuracy of clustering with 3 PCs
Accuracy <- sum(diag(Comparison_table))/sum(Comparison_table)
Accuracy
# When clustering done using first 3 principle components, 7 rows got incorrectly clustered. Accuracy is 96%. 
