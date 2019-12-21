



data = read.csv(file.choose())
View(data)
str(data)
#Converting into factor
data[c(1:11)] = lapply(data[c(1:11)], factor)
str(data)
#We need data in sparse matrix form
#Read the data set as transection.
#Using the arules library
#install.packages("arules")
library(arules)
trans = as(data,"transactions")

View(trans)
summary(trans)


#Plot showing the most buy items
itemFrequencyPlot(x=trans,topN=15)

#Building Rule
#Support of 3 items buy in day
rule = apriori(data=trans,parameter = list(support=0.04,confidence=0.4))

#Inspecting the rules
inspect(sort(rule,by="lift")[1:10])

