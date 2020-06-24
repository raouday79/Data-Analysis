install.packages("recommenderlab")
library(recommenderlab)
library(reshape2)

####### Example: Data generated in class #####

ratings_list <- read.csv(choose.files())

head(ratings_list)

ratings_list <- ratings_list[,2:4]
head(ratings_list)
dim(ratings_list)

## covert to matrix format
?acast
ratings_matrix <- as.matrix(acast(ratings_list, user~movie, fun.aggregate = mean))
dim(ratings_matrix)

## recommendarlab realRatingMatrix format
R <- as(ratings_matrix, "realRatingMatrix")
?realRatingMatrix

rec1 = Recommender(R, method="UBCF") ## User-based collaborative filtering
rec2 = Recommender(R, method="IBCF") ## Item-based collaborative filtering
rec3 = Recommender(R, method="SVD")
rec4 = Recommender(R, method="POPULAR")
rec5 = Recommender(binarize(R,minRating=2), method="UBCF") ## binarize all 2+ rating to 1

## create n recommendations for a user
uid = "Ravi"

movies <- subset(ratings_list, ratings_list$user==uid)
print("You have rated:")
movies

print("recommendations for you:")
prediction1 <- predict(rec1, R[uid], n=5) 
as(prediction1, "list")

prediction2 <- predict(rec2, R[uid], n=3) 
as(prediction2, "list")

prediction3 <- predict(rec3, R[uid], n=4) 
as(prediction3, "list")

prediction4 <- predict(rec4, R[uid], n=4)
as(prediction4, "list")

prediction5 <- predict(rec5, R[uid], n=2) 
as(prediction5, "list")

