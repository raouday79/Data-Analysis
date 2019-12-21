
# dataset = SalaryData_Train.csv and SalaryData_Test.csv
data_train = read.csv(file.choose())
data_test = read.csv(file.choose())
View(data_train)
View(data_test)
str(data_train)
str(data_test)
install.packages("e1071")
library("e1071")

#Scaling the data set...
data_train[c(1,4,10,11,12)] = scale(data_train[c(1,4,10,11,12)])
data_test[c(1,4,10,11,12)] = scale(data_test[c(1,4,10,11,12)])

classifier = svm(formula=data_train$Salary~.,data=data_train,type="C-classification",kernal="radial")
y_pred = predict(classifier,newdata = data_test[-14])
cm = table(data_test[,14],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#0.8472776
classifier2 = svm(formula=data_train$Salary~.,data=data_train,type="C-classification",kernal="polynomial")
y_pred2 = predict(classifier2,newdata = data_test[-14])
cm2 = table(data_test[,14],y_pred2)
cm2
accuracy2 = sum(diag(cm2)/sum(cm2))
accuracy2
#0.8472776


classifier3 = svm(formula=data_train$Salary~.,data=data_train,type="C-classification",kernal="linear")
y_pred3 = predict(classifier3,newdata = data_test[-14])
cm3 = table(data_test[,14],y_pred3)
cm3
accuracy3 = sum(diag(cm3)/sum(cm3))
accuracy3
#0.8472776



