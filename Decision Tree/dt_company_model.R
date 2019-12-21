data = read.csv(file.choose())
View(data)
str(data)
summary(data)


library(caTools)
set.seed(123)
split = sample.split(data,SplitRatio = 0.8)
training_data = subset(data,split==TRUE)
testing_data = subset(data,split==FALSE)

View(training_data)
library(rpart)
regression = rpart(formula = training_data$Sales~.,data=training_data,control = rpart.control(minsplit = 2))
y_pred = predict(regression,newdata = testing_data[,-1])


install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(regression, box.palette="RdBu", shadow.col="gray", nn=TRUE)
plot(regression)
text(regression)