library(caret)

# import data and clean it up
podatki = read.csv("podatki.csv", fileEncoding = "UTF-8")

podatki$X4 = gsub('“|”', '', as.character(podatki$X4))
podatki$X4[podatki$X4 == "YES"] <- "DA"
podatki$X4[podatki$X4 == "NO"] <- "NE"

# if `X4` were already a factor, we would get rid on unwanted levels with: 
# podatki$X4 <- droplevels(podatki$X4)
podatki$X4 <- as.factor(podatki$X4)
podatki$X6 = as.numeric(gsub('“|”', '', as.character(podatki$X6)))
podatki$Y <- as.factor(podatki$Y)

# create a test set and a train set 
train_index <- createDataPartition(podatki$Y, p = 0.8, list = FALSE)
podatki.train <- podatki[train_index, ]
podatki.test <- podatki[-train_index, ]

# train a `KNN` method, where all the columns other than `X` are predictors
podatki.knn <- train(Y ~. -X, 
                     data = podatki.train, 
                     method = "knn", 
                     tuneGrid = data.frame(k = 3:7),
                     preProcess = c("center", "scale")
)

# Access the correctness of the model
d.train <- dim(podatki.train)
d.test <- dim(podatki.test)

predict.train <- predict(podatki.knn, newdata = podatki.train)
predict.test <- predict(podatki.knn, newdata = podatki.test)

correctness.train <- {
  sum(predict.train == podatki.train[, d.train[2]]) / d.train[1]
}
correctness.test <- {
    sum(predict.test == podatki.test[, d.test[2]]) / d.test[1]
}

print(sprintf("Train correctness: %.2f; Test correctness: %.2f ", 
              correctness.train, correctness.test))
