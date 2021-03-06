#Dataset=groceries.csv
data = read.csv(file.choose())
data=data[-1,]
View(data)
data = data[,6:15]
View(data)
str(data)

#Converting into factor
data[c(1:10)] = lapply(data[c(1:10)], factor)

#name <- "movies_data.csv"
#write.csv(data,file=name)
#We need data in sparse matrix form
#Read the data set as transection.
#Using the arules library
#install.packages("arules")
library(arules)
library(arulesViz)
trans = as(data,"transactions")
#dataset = read.transactions(name,cols =2 , sep=",",rm.duplicates = TRUE)
View(trans)
summary(trans)
#Plot showing the most
itemFrequencyPlot(x=trans,topN=10)
#Building Rule
#Support of 2
rule = apriori(data=trans,parameter = list(support=0.02,confidence=0.3))

#Inspecting the rules
inspect(sort(rule,by="lift")[1:4])
plot(rule)

#Support of 5
rule2 = apriori(data=trans,parameter = list(support=0.5,confidence=0.4))
#23550 rules
#Inspecting the rules
inspect(sort(rule2,by="lift")[1:4])
plot(rule2)
#graph plot of rule
plot(rule2,method="graph",max=10)
#people who watch harry potter 1  also watch harry potter 2 vice versa is also true from the above rule.





