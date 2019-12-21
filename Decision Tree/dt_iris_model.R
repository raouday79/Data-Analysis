#install.packages("rpart")
#install.packages("caTools")
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

#Scaling the dataset
#training_data[1:4]=scale(training_data[1:4])
#testing_data[1:4]=scale(testing_data[1:4])

#Building the model without scale
install.packages("rpart")
library(rpart)
classifier = rpart(formula = iris$Species~.,iris)
y_pred = predict(classifier,testing_data[-5],type = "class")
View(y_pred)
cm = table(testing_data[,5],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#Getting accuracy = 0.9666667

#Ploting the tree
plot(classifier)
text(classifier)
