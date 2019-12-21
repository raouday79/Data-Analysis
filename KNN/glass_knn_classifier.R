#Dataset = glass.csv
data = read.csv(file.choose())
View(data)
str(data)
 
#Converting the dependent variable into the factor
data$Type = factor(data$Type)
str(data)



#Spliting the data into training and testing
library(caTools)
set.seed(222)
split = sample.split(data,SplitRatio = 0.8)
traning_data = subset(data,split==TRUE)
testing_data = subset(data,split==FALSE)

#Scaling the data set
traning_data[-10] = scale(traning_data[-10])
testing_data[-10] = scale(testing_data[-10])

glass_pred <- knn(train = traning_data[,-10],test=testing_data[,-10],cl=traning_data[,10],k=5)

#Confusion matrix
cm = table(testing_data[,10],glass_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#k=1 then 0.6046512
#k=3 then 0.6744186
#k=5 then 0.6976744
#k=7 then 0.6046512

