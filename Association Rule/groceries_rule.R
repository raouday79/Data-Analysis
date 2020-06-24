#Dataset=groceries.csv
data = read.csv(file.choose(),header = FALSE)
View(data)

#We need data in sparse matrix form
#Read the data set as transection.
#Using the arules library
install.packages("arules")
library(arules)
dataset = read.transactions(file.choose(),sep=",",rm.duplicates = TRUE)
View(dataset)
summary(dataset)

library(arulesViz)

#Plot showing the most buy items
itemFrequencyPlot(x=dataset,topN=15)

#Building Rule
#Support of 3 items buy in day... rule for the most frequent but items
rule = apriori(data=dataset,parameter = list(support=0.003,confidence=0.4))

#Inspecting the rules
inspect(sort(rule,by="lift")[1:10])


#Rule for less frequent item in a day

rule2 = apriori(data=dataset,parameter = list(support=0.01,confidence=0.3))

#Inspecting the rules
inspect(sort(rule2,by="lift")[1:10])

plot(rule2)  # less support has high lift value and low confidence


#plotting the bubble chart
plot(rule2,method="grouped")

#graph plot of rule
plot(rule2,method="graph",max=10)

