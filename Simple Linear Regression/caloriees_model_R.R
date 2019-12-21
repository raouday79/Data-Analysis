getwd()

#Dataset = calories_consumed
data = read.csv(file.choose())
View(data)
cor(data$Weight.gained..grams.,data$Calories.Consumed)
plot(data)


#BUilding model
reg = lm(data$Weight.gained..grams.~.,data=data)
summary(reg)
# Adjusted R-squared Value close to 0.8882

plot(reg)

library(Metrics)
rmse(data$Weight.gained..grams.,predict(reg,data=data$Calories.Consumed))
# rmse = 103.3025

#Applying mathmatical transformation

#------------------------------------------------------------------------------------
reg1 = lm(log(data$Weight.gained..grams.)~log(data$Calories.Consumed),data = data)
# Adjusted R-squared Value close to 0.8337
summary(reg1)
rmse(data$Weight.gained..grams.,predict(reg1,data=data$Calories.Consumed))
# RMSE value 476.4366



