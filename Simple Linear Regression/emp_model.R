getwd()

#Dataset = emp_data.csv
data = read.csv(file.choose())
View(data)
cor(data$Salary_hike,data$Churn_out_rate)
plot(data)
 #corr -0.9117216

#BUilding model
reg = lm(data$Churn_out_rate~data$Salary_hike,data=data)
summary(reg)
# Adjusted R-squared Value close to 0.8101
plot(reg)

library(Metrics)
rmse(data$Churn_out_rate,predict(reg,data=data$Salary_hike))
# rmse = 3.997525

#Applying mathmatical transformation

#------------------------------------------------------------------------------------
reg1 = lm(log(data$Churn_out_rate)~log(data$Salary_hike),data = data)
summary(reg1)
# Adjusted R-squared Value close to 0.8752
rmse(data$Churn_out_rate,predict(reg1,data=data$Salary_hike))
# RMSE value 69.29027



