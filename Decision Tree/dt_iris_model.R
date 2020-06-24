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
classifier = rpart(formula = training_data$Species~.,training_data,method = 'anova')
y_pred = predict(classifier,testing_data[-5],type = "class")
#View(y_pred)
cm = table(testing_data[,5],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#Getting accuracy = 0.9666667

#Ploting the tree
plot(classifier)
text(classifier)


# Using the C50 libraray
library(caret)
library(C50)

model <- C5.0(training$Species~.,data = training,trails = 40)# here boosting iteration will be 40
summary(model)
pred <- predict.C5.0(model,testing[,-5])
a <- table(testing$Species,pred)
a
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  fittree<-C5.0(training1$Species~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc



