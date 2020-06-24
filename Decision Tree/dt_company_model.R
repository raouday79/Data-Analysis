data = read.csv(file.choose())
View(data)
str(data)
summary(data)

hist(data$Sales)# we can see the data follow the normal distribution...

#Converting Sales into Low and High
data$Sales = ifelse(data$Sales>5,"Low","High")
#making feature as factor
data$Sales = factor(data$Sales)

# bar plot of low sale and high sale
barplot(table(data$Sales),xlab = "Sales Category",ylab = "Sales Count")

library(caTools)
set.seed(123)
split = sample.split(data,SplitRatio = 0.8)
training_data = subset(data,split==TRUE)
testing_data = subset(data,split==FALSE)

#Building classifier using C50
library(C50)
model <- C5.0(formula=training_data$Sales~.,data = training_data,trails = 50) # Setting the number of iterations

# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing_data[,-1])
a <- table(testing_data$Sales,pred)
a
sum(diag(a)/sum(a))
plot(model)



#Building classifier using rpart

View(training_data)
library(rpart)
regression = rpart(formula = training_data$Sales~.,data=training_data,control = rpart.control(minsplit = 2))
y_pred = predict(regression,newdata = testing_data[,-1])

x <- table(testing_data$Sales,pred)
x
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(regression, box.palette="RdBu", shadow.col="gray", nn=TRUE)
plot(regression)
text(regression)
