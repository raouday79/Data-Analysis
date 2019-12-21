#Dataset = Fraud_check.csv
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
#training_data[4:5]=scale(training_data[4:5])
#testing_data[4:5]=scale(testing_data[4:5])

library(randomForest)
classifier = randomForest(x=training_data[,-3],y=training_data[,3],data=training_data,ntree = 20)
y_pred  = predict(classifier,testing_data[,-3])
cm = table(testing_data[,3],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
# Accuracy = 0.795
# After scaling accuracy reduce to 0.755
# Every time building the random forest the accuracy changes even the nTree is same







