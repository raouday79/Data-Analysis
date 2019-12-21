install.packages("AER")
install.packages("ElemStatLearn")
library(ElemStatLearn)
library(AER)
library(car)
data = read.csv(file.choose())
View(data)

#Data Pre-processing
#Coverting to dummy variables - Gender
data$gender = factor(data$gender,levels = c("male","female"),labels = c(0,1))
#Coverting to dummy variables - Childern
data$children = factor(data$children,levels = c("no","yes"),labels = c(0,1))

View(data)
# if affair==0 then 0 else it is 1 for any value
data$affairs = ifelse(data$affairs==0,0,1)
View(data)

pairs(data)
# Removing first column
data = subset(data,select = -c(X))


#Applying backward elimination
model = glm(data$affairs~.,data=data,family = "binomial")
summary(model)

#Dropping variable education
model_2 = glm(data$affairs~data$age+data$gender+data$yearsmarried+data$children+data$religiousness+data$occupation+data$rating,data=data,family = "binomial")
summary(model_2)

#Dropping variable occupation
model_3 = glm(data$affairs~data$age+data$gender+data$yearsmarried+data$children+data$religiousness+data$rating,data=data,family = "binomial")
summary(model_3)

#Dropping variable Children
model_4 = glm(data$affairs~data$age+data$gender+data$yearsmarried+data$religiousness+data$rating,data=data,family = "binomial")
summary(model_4)

vif(model_4)
cor(data)
# getting probability in 0 and 1-----------------------------------------
prob = predict(model_4,type='response',newdata = data)
result = ifelse(prob>0.5,1,0)
result 

#Model performance by confusion matrix
prob <- predict(model_4,type=c("response"),data)
prob
confusion<-table(prob>0.50,data$affairs)
confusion

#Getting Accuracy of 77.03%
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
