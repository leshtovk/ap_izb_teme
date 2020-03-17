library(caret)
data(iris)

# create a train set and a test set (60 - 40 split)
iris.part <- createDataPartition(iris$Species, p = 0.6, list = FALSE)
iris.train <- iris[iris.part, ]
iris.test <- iris[-iris.part, ]

# train the classifier
iris.knn <- train(Species ~., data = iris.train, 
                  method = "knn", tuneGrid = data.frame(k = 1:30))

# plot `neighbors` vs `accuracy`
# plot(iris.knn)
# plot(iris.knn, ylim = c(0, 1))

# compare the predictions of the classifier to the test data
d = dim(iris.test)
iris.knn.predictions <- predict(object = iris.knn, newdata = iris.test)
iris.knn.correct <- sum(iris.knn.predictions == iris.test[, d[2]])
iris.knn.correctness <- iris.knn.correct / d[1]
print(sprintf("Correctness of the model on the test set: %.3f",
              iris.knn.correctness))