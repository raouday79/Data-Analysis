#Dataset =  Wine.csv
data = read.csv(file.choose())

View(data)


#Scaling the dataset
data[-1] = scale(data[-1])
View(data)


library(caret)
library(e1071)

pca = preProcess(x=data[-1],method = "pca",pcaComp = 2)
pca_data = predict(pca,data[-1])
View(pca_data)

#Appliying K-Means Clustering to pca_data 
set.seed(123)
wcss <- vector()

for (i in 1:10){
  wcss[i] <- sum(kmeans(pca_data,i)$withinss)
  
}
#Plotting the elbow plot
plot(1:10,wcss,type ="b",main=paste("Cluster of Wine"),xlab = "No. of Cluster",ylab = "WCSS" )
#from the elbow flot optimum number of cluster id K=3


kclust = kmeans(pca_data,3,iter.max = 100,nstart = 10)

l = kclust$cluster
pca_data["Cluster"] <- l
View(pca_data)
table(pca_data$Cluster)
# There are 3 cluster
# Cluster 1 contain 64
# Cluster 2 contain 65
# Cluster 3 contain 49








#---------------------------------------------------------------------------------
#Applying K-mean of raw data
View(data)
set.seed(33)
wcss_2 <- vector()

for (i in 1:10){
  wcss_2[i] <- sum(kmeans(data[-1],i)$withinss)
  
}
#Plotting the elbow plot
plot(1:10,wcss_2,type ="b",main=paste("Cluster of Wine"),xlab = "No. of Cluster",ylab = "WCSS" )
#from the elbow flot optimum number of cluster id K=3
#Getting the same result without pca
# i.e optimum cluster = 3










