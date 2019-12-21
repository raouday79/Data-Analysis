data("iris")
View(iris)

str(iris)
#int data type with one is factor


#Spliting data into trainig and testing
library(caTools)
set.seed(123)
split = sample.split(iris,SplitRatio = 0.8)
training_data = subset(iris,split==TRUE)
testing_data = subset(iris,split==FALSE)

#Building Random Forest
#install.packages("randomForest")
library(randomForest)
classifier = randomForest(x=training_data[,-5],y=training_data[,5],data=training_data,ntree = 3)
y_pred  = predict(classifier,testing_data[-5])
cm = table(testing_data[,5],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#nTree=10 getting  accuracy=93
#nTree=5 getting accuracy=96
#nTree=3 getting accuracy=90

plot(classifier)
text(classifier)



