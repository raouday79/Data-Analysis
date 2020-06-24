



data = read.csv(file.choose())
View(data)
str(data)
#Converting into factor
data[c(1:11)] = lapply(data[c(1:11)], factor)
str(data)
#We need data in sparse matrix form
#Read the data set as transection.
#Using the arules library
install.packages("arules")
library(arules)
trans = as(data,"transactions")

View(trans)
summary(trans)


#Plot showing the most buy items
itemFrequencyPlot(x=trans,topN=10)
#from the above plot most frequenct bookbought was ItalAtlas, ItalArt, Florence, ItalCook

#Building Rule
#Support of 3 items buy in day
rule = apriori(data=trans,parameter = list(support=0.04,confidence=0.4))

#Inspecting the rules
inspect(sort(rule,by="lift")[1:10])

# Genearating the moore stronger rule by increasing the confidence value.

rul2 = apriori(data=trans,parameter = list(support=0.2,confidence=0.5))
inspect(sort(rul2,by="lift")[1:10])


# ule for the more frequent buy books...
rule3 = apriori(data=trans,parameter = list(support=0.5,confidence=0.6))
inspect(sort(rule3,by="lift")[1:10])

#install.packages("aruleViz")
library(arulesViz)
plot(rul2)
#From the above plot we can anlyze there is high lift for low support value. for the confidence value between 0.7 to 0.8

dia1 = plot(rul2,measure = c("support","lift"),shading = "confidence",interactive = TRUE)

arules::inspect(rul2)
#plotting the bubble chart
plot(rul2,method="grouped")

#graph plot of rule
plot(rul2,method="graph",max=20)






