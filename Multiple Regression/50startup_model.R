data = read.csv(file.choose())
View(data)
str(data)
#One hot encoding
#Creating the dummy  variables
data$State = factor(data$State,levels = c("New York", "California", "Florida"), labels = c(1,2,3))
View(data)
cor(data[,-4])
library(car)
library(Metrics)
cor2pcor(cor(Cars))
pairs(data)

#State variable converted into factor with 3 labels 1,2,3  for state New York, California, Florida
str(data)



#-------------------------------------------------------------------------
# Considering all the variable 
# Adjusted R2 value is 0.9452
# In this Administration , MArketing Spend, State are insignificant value
model1 = lm(formula = data$Profit~.,data=data)
summary(model1)

#Checking the collinearity
vif(model1)
# All tha value are less then 10. So no collinearity exist

#-------------------------------------------------------------------------
#Plotting the AVPLOT to check which are insignificant after model building.
avPlots(model1,id.n=2,id.cex=0.7,)
# Found that State has no relation with Y.

#------------------------------------------------------------------------
model_2 = lm(formula = data$Profit~data$R.D.Spend,data=data)
summary(model_2)
# Found Same adjusted R2 value 0.9454

model_3 = lm(formula = data$Profit~data$R.D.Spend+data$Marketing.Spend,data=data)
summary(model_3)
#increase in significance level
#Adjusted R2 value is 0.9483


#--------------------------------------------------------------------------
model_4 = lm(formula = data$Profit~data$R.D.Spend+data$Marketing.Spend+data$Administration,data=data)
summary(model_4)
#Both marketing and Administration become insignificant
#Adjusted R2 value is 0.9475


#--------------------------------------------------------------------------
#Best Model with Adjusted R2 value 0.9483
rmse(data$Profit,predict(model_3,data=data))
#RMSE is $8881.886
hist(residuals(model_3))
plot(model_3)

