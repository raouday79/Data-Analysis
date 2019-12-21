#dataset=EastWestAirlines
data = read.csv(file.choose())
View(data)
data2 = data[,-1]
str
View(data2)

#Scaling the data points
data2 <- scale(data2)


#calculating distance matrix
dist = dist(data2,method = "euclidean")
dist
#Building HClustering
hc = hclust(dist,method = "complete")
plot(hc,main = "Airline",
     xlab = "Passender",
     ylab = "Distance")



#K mean-Clustering
set.seed(123)
wcss <- vector()

for (i in 1:10){
  wcss[i] <- sum(kmeans(data2,i)$withinss)
  
}
#Plotting the elbow plot
plot(1:10,wcss,type ="b",main=paste("Cluster of Customer"),xlab = "No. of Cluster",ylab = "WCSS" )
#from the elbow flot optimum number of cluster id K=3

kclust = kmeans(data2,3,iter.max = 100,nstart = 10)

l = kclust$cluster
data["Cluster"] <- l
View(data)
table(data$Cluster)
# There are 3 cluster
# Cluster 1 contain 2574
# Cluster 2 contain 1259
# Cluster 3 contain 166
