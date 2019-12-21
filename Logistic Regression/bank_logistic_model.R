#Dataset=bank-full.csv
data <- read.csv(file.choose(),sep=";")
View(data)
str(data)

model_1 = glm(formula = data$y~.,family = binomial,data = data)
summary(model_1)
#Removing the insinificant

model_2 = glm(formula = data$y~data$job+data$marital+data$education+data$default+data$balance
              +data$housing+data$loan+data$contact+data$day+data$month
              +data$duration+data$campaign+data$pdays+data$previous
              +data$poutcome,
              family = binomial,
              data = data
              )
summary(model_2)

model_3 = glm(formula = data$y~data$job+data$marital+data$education+data$default+data$balance
              +data$housing+data$loan+data$contact+data$day+data$month
              +data$duration+data$campaign
              +data$poutcome,
              family = binomial,
              data = data
)
summary(model_3)



model_4 = glm(formula = data$y~data$job+data$marital+data$education+data$balance
              +data$housing+data$loan+data$contact+data$day+data$month
              +data$duration+data$campaign
              +data$poutcome,
              family = binomial,
              data = data
)
summary(model_4)

# Confusion matrix table 
prob <- predict(model_4,type=c("response"),data)
prob
confusion<-table(prob>0.5,data$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
#Getting Accuracy of 0.9019486
