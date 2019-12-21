#Dataset = Computer_Data.csv
data = read.csv(file.choose())
View(data)
str(data)

#Removing the first column
data= data[,-1]
str(data)
#Cd,multi,premium are already in factor

#Corealtion matrix
factor_data <- c(6,7,8)
cor(data[,-factor_data])
pairs(data[,-factor_data])

model1 = lm(formula = data$price~.,data=data)
summary(model1)
#All values are significant
vif(model1)
# All tha value are less then 10. So no collinearity exist

#Best Model with Adjusted R2 value 0.7752
rmse(data$price,predict(model1,data=data))
#RMSE is $8881.886
hist(residuals(model1))
plot(model1)

