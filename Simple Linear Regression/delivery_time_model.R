#Dataset = delivery_time.csv

getwd()
library("readr")
#install.packages("Metrics")
library(Metrics)
data = read_csv(file.choose())
View(data)
cor(data$`Delivery Time`,data$`Sorting Time`)
# getting corelation 0.8259973
plot(data)
summary(data)
hist(data$`Delivery Time`)
# Building linear simple model
reg = lm(data$`Delivery Time`~data$`Sorting Time`,data = data)
# Adjusted R-squared Value close to 0.6655
summary(reg)
rmse(data$`Delivery Time`,predict(reg,data=data$`Sorting Time`))
# RMSE value 2.79165

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
reg1 = lm(log(data$`Delivery Time`)~log(data$`Sorting Time`),data = data)
# Adjusted R-squared Value close to 0.7602
summary(reg1)
rmse(data$`Delivery Time`,predict(reg1,data=data$`Sorting Time`))
# RMSE value 14.79194

#-------------------------------------------------------------------------------------
reg2 = lm(sqrt(data$`Delivery Time`)~data$`Sorting Time`,data = data)
# Adjusted R-squared Value close to 0.948
summary(reg2)
rmse(data$`Delivery Time`,predict(reg2,data=data$`Sorting Time`))
# RMSE value 13.52335
