library(recommenderlab)
library(reshape2)
ratings_list <- read.csv(choose.files())

head(ratings_list)
ratings_list <- ratings_list[,2:6]
head(ratings_list)
dim(ratings_list)


## covert to matrix format
?acast
ratings_matrix <- as.matrix(acast(ratings_list,ratings_list$users...1. ~ratings_list$Book.Title, fun.aggregate = mean))
dim(ratings_matrix)


## recommendarlab realRatingMatrix format
R <- as(ratings_matrix, "realRatingMatrix")
?realRatingMatrix

rec1 = Recommender(R, method="UBCF") ## User-based collaborative filtering
rec2 = Recommender(R, method="IBCF") ## Item-based collaborative filtering
rec3 = Recommender(R, method="SVD")
rec4 = Recommender(R, method="POPULAR")
rec5 = Recommender(binarize(R,minRating=2), method="UBCF")

uid = "1"

movies <- subset(ratings_list, ratings_list$user==uid)
print("You have rated:")
movies

prediction4 <- predict(rec4, R[uid], n=4)
as(prediction4, "list")
