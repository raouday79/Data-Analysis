#Dataset= SalaryData_Test.csv and SalaryData_Train.csv
test_data <- read.csv(file.choose())
train_data <-  read.csv(file.choose())
View(train_data)
str(train_data)

#Scaling the data set...
train_data[c(1,4,10,11,12)] = scale(train_data[c(1,4,10,11,12)])
test_data[c(1,4,10,11,12)] = scale(test_data[c(1,4,10,11,12)])

#Building Classifier
library(e1071)
classifier = naiveBayes(x = train_data[,-14],y = train_data[,14])
y_pred = predict(classifier,test_data[,-14])
#Building confusion matrix
cm = table(test_data[,14],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#0.8196547