library(e1071)
#dataset = forestfire.csv
data = read.csv(file.choose())
View(data)
data2 = data[-c(12:30)]
View(data2)
str(data2)

#Scaling the features
data2[3:11] = scale(data2[3:11])


#splitting the data into training and testing
install.packages("caTools")
library(caTools)
set.seed(111)
split = sample.split(data2,SplitRatio = 0.80)
training_set = subset(data2, split == TRUE)
test_set = subset(data2, split == FALSE)
View(training_set)
View(test_set)


library(e1071)
classifier = svm(formula=training_set$size_category~.,data=training_set,type="C-classification",kernal="radial")
y_pred = predict(classifier,newdata = test_set[-12])
cm = table(test_set[,12],y_pred)
accuracy =sum(diag(cm)/sum(cm))
accuracy
#0.8153846

classifier2 = svm(formula=training_set$size_category~.,data=training_set,type="C-classification",kernal="linear")
y_pred2 = predict(classifier2,newdata = test_set[-12])
cm2 = table(test_set[,12],y_pred2)
accuracy2 =sum(diag(cm2)/sum(cm2))
accuracy2
#0.8153846



