#Dataset = Salary_Data.csv

getwd()
library("readr")
install.packages("Metrics")
library(Metrics)
data = read_csv(file.choose())
View(data)
attach(data)
cor(data$Salary,data$YearsExperience)
# getting corelation 0.9782416
plot(data)
summary(data)
hist(data$YearsExperience)
# Building linear simple model
reg = lm(data$Salary~data$YearsExperience,data = data)
# Adjusted R-squared Value close to 0.9554
summary(reg)
rmse(Salary,predict(reg,data=data))


# getting ND for Residual error
plot(reg)
# confident interval  with 1-alpha 95
confint(reg,level = 0.95)
plot(reg)
# what is differnce between interval predict and confidence
predict(reg,interval = "predict")
predict(reg,interval="confidence")

#Applying mathmatical transformation

#------------------------------------------------------------------------------------
reg1 = lm(log(Salary)~log(YearsExperience),data = data)
# Adjusted R-squared Value close to 0.9018
summary(reg1)
rmse(Salary,predict(reg1,data=data))
# RMSE value 80630.27

#-------------------------------------------------------------------------------------
reg2 = lm(sqrt(Salary)~YearsExperience,data = data)
# Adjusted R-squared Value close to 0.948
summary(reg2)
rmse(Salary,predict(reg2,data=data))

# Getting RMSE value very large of about 80.369.63 ?????????????????
