library(caret)

# create a reproducible dataset
set.seed(123)

N <- 5000
m <- 10

xs <- matrix(rnorm(N * m), ncol = 10)
xs <- pmin(xs, 1)
xs <- pmax(xs, -1)

data <- as.data.frame(xs)

Y <- { 
  1 + xs[,1] - 2 * xs[,2] + 
  (xs[,4] * xs[,5] - xs[,3] * xs[,6]) / 3 + 
  rnorm(N, mean = 0, sd = 0.01) 
}

data$Y <- Y

# change the names of the predictors from `Vi` to `Xi`
# if we had used `data <- data.frame(xs)`,  
# the names would have already been `Xi` instead of `Vi`
columnNames <- character(m + 1)
for (i in 1:m) {
  columnNames[i] <- sprintf("X%d", i)
}
columnNames[m + 1] <- "Y"
colnames(data) <- columnNames

# make `Y` a nominal variable with possible values "1" and "0"
Y <- data$Y
ones <- Y > mean(Y)
Y[ones] <- 1
Y[!ones] <- 0
data$Y <- Y

data2 <- data
data2$Y <- as.factor(data2$Y)

################################################################################

# make a test set and a training set

trainIndices <- createDataPartition(data2$Y, p = 4/5, list = FALSE)

trainSet1 <- data[trainIndices, ]
testSet1 <- data[-trainIndices, ]

trainSet2 <- data2[trainIndices, ]
testSet2 <- data2[trainIndices, ]

################################################################################

# train a "naive" linear model
# model1 <- train(Y ~., data = trainSet1, model = "lm")

# train a logistical regression model
model2 <- train(Y ~., data = trainSet2, model = "glm")

################################################################################

# prepare functions

# RMSE
error <- function(Y, Y_predicted){
  sqrt(mean((Y == Y_predicted)^2))
}

modelError <- function(model, data) {
  d <- dim(data)
  predictions <- predict(model, newdata = data)
  error(data[, d[2]], predictions)
}

print(sprintf("Accuracy of log. regression: %.3f", modelError(model2, testSet2)))

