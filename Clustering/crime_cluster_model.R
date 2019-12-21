
#Dataset = crime_data.csv
data = read.csv(file.choose())

View(data)
str(data)
#data = data[,-c(1)]
data = scale(data[,2:5])
d <- dist(data, method = "euclidean")
d
fit <- hclust(d, method="single")
plot(fit) # display dendrogram
plot(fit, hang=-1)
l
rect.hclust(fit, k=7, border="red")
groups <- cutree(fit, k=7) # cut tree into 5 clusters

cluster<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(data, cluster)
View(final)
