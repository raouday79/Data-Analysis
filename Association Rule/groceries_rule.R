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
#Plot showing the most buy items
itemFrequencyPlot(x=dataset,topN=15)

#Building Rule
#Support of 3 items buy in day
rule = apriori(data=dataset,parameter = list(support=0.003,confidence=0.4))

#Inspecting the rules
inspect(sort(rule,by="lift")[1:10])
