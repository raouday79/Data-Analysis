#Dataset = ToyotaCorolla.csv
data = read.csv(file.choose())
View(data)
Corolla<-data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
str(Corolla)

cor(Corolla)


#Building model
model1 = lm(formula = Corolla$Price~.,data=Corolla)
summary(model1)

#Found that CC and Doors have least significant.
#Dropping these columns
model_2 = lm(formula = Corolla$Price~Corolla$Age_08_04+Corolla$KM+Corolla$HP+Corolla$Gears+Corolla$Quarterly_Tax+Corolla$Weight,data=Corolla)
summary(model_2)
#Adjusted R value = 0.863

rmse(data$Price,predict(model_2,data=Corolla))
#RMSE is $8881.886
hist(residuals(model_2))
plot(model_2)

