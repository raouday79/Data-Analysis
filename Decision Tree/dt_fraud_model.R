

data = read.csv(file.choose())

str(data)
summary(data)
View(data)

#Converting taxable income to Risky and good
data$Taxable.Income = ifelse(data$Taxable.Income<=30000,"Risky","Good")
View(data)
str(data)
#making feature as factor
data$Taxable.Income = factor(data$Taxable.Income)
str(data)


#Spliting data into trainig and testing
library(caTools)
set.seed(123)
split = sample.split(data,SplitRatio = 0.8)
training_data = subset(data,split==TRUE)
testing_data = subset(data,split==FALSE)

#Scaling the dataset
training_data[4:5]=scale(training_data[4:5])
testing_data[4:5]=scale(testing_data[4:5])


#Building classifier
classifier = rpart(formula = training_data$Taxable.Income~.,data=training_data)
y_pred = predict(classifier,newdata = testing_data[,-3],type="class")
#here it gives the probablity of which class will be.
#it give the class use type="class" in predict as argument
View(y_pred)

#Confusion matrix
cm = table(testing_data[,3],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy

#Accuracy is 0.76
#After scaling accuracy increase to 0.77

#Ploting the tree
library(rpart.plot)
rpart.plot(classifier, box.palette="RdBu", shadow.col="gray", nn=TRUE)
plot(classifier)
text(classifier)
