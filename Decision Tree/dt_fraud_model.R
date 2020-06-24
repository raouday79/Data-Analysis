

data = read.csv(file.choose())
hist(data)

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


install.packages("RColorBrewer")
library(RColorBrewer)
barplot(table(data$Taxable.Income),col=brewer.pal(2,"Dark2"),xlab = "Class",ylab = 'Frequency')
#from the above bar plot we can see that the class good is dominating over the risky class
#Spliting data into trainig and testing
library(caTools)
set.seed(123)
split = sample.split(data,SplitRatio = 0.8)
training_data = subset(data,split==TRUE)
testing_data = subset(data,split==FALSE)

#As decision tree is not based on distance calculation the scale is not required.
#Scaling the dataset
training_data[4:5]=scale(training_data[4:5])
testing_data[4:5]=scale(testing_data[4:5])


#Building classifier using C50
library(C50)
model <- C5.0(formula=training_data$Taxable.Income~.,data = training_data,trails = 50)

# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing_data[,-3])
a <- table(testing_data$Taxable.Income,pred)
a
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  set.seed(123)
  split = sample.split(data,SplitRatio = 0.8)
  training1 = subset(data,split==TRUE)
  testing = subset(data,split==FALSE)
  
  fittree<-C5.0(training1$Taxable.Income~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-3])
  a<-table(testing$Taxable.Income,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc

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
