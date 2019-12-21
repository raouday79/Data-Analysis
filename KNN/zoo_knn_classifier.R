#Dataset = Zoo.csv
data = read.csv(file.choose())
View(data)
str(data)
# making dependent variable as categorical variable.
data$type = factor(data$type)
View(data)
str(data)
#Removing the animal name variable.
data = data[-1]

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#No need to normalize the data coz already in 1's and 0's formate
data_norm <- as.data.frame(lapply(data[1:16], norm))
View(data_norm)

#Not a random sampling technique
#Splitting data into the tarin and the test.
d_train  = data_norm[1:80,]
d_test = data_norm[81:101,]

#Getting train and test label from the original data.
d_train_label = data[1:80,17]
d_test_label =data[81:101,17]

library("class")
library("caret")

#Building the  model.
zoo_pred <- knn(train = d_train, test = d_test, cl = d_train_label, k=1)
library("gmodels")
#Validating the model using the cross table method.
CrossTable( x =  d_test_label, y = zoo_pred)
# checking for different k value

#80.95 k=1  17/21
#76.19 k=3  16/21
#76.19 k=5  16/21
#76.19 k=7  16/21
#76.19 k=11 16/21




#By Confusion Matrix
cm = table(d_test_label,zoo_pred)
cm
