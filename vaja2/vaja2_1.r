# library(caret)

# create the dataset

# make the results reproducible
set.seed(123)

# create the data
N <- 5000
m <- 10

A <- matrix(rnorm(N * m), ncol = m)
y <- { 1 + A[, 1] - 2 * A[, 2] + 
       (A[, 4] * A[, 5] - A[, 3] * A[, 6]) / 3 + 
       rnorm(N, 0, 0.01) }

data <- data.frame(A)
data$y <- y

################################################################################

# create a training set and a test set
trainIndices <- createDataPartition(data$y, p = 0.8, list = FALSE)
trainingSet <- data[trainIndices, ]
testSet <- data[-trainIndices, ]

# train a linear regression model
model <- train(y ~., data = trainingSet, method = "lm")

# positively and negatively correlated features 
indicesPos <- which(model$finalModel$coefficients[-1] > 0)
indicesNeg <- which(model$finalModel$coefficients[-1] < 0)

print(sprintf("Positively correlated are %s", 
              paste(colnames(data)[indicesPos], collapse = ", ")
      )
)

print(sprintf("Negatively correlated are %s", 
              paste(colnames(data)[indicesNeg], collapse = ", ")        
      )
)

################################################################################  

# Backward selection

# prepare functions

# RMSE
error <- function(y, y_pred){
  sqrt(mean((y - y_pred)^2))
}

modelError <- function(model, data){
  d <- dim(data)
  predictions <- predict(model, newdata = data)
  error(data[, d[2]], predictions)
}

makeFormula <- function(names) {
  as.formula(paste("y ~", paste(names, collapse = "+")))
}


# do backward selection
print("Starting backward feature selection")

d <- dim(data)
features <- colnames(data)[-d[2]]

# create a vector of the smallest possible RMSE's, where the element
# at index `k` represents the case where `k` predictors are considered 
optErrors <- rep(0, length(features))
optErrors[length(features)] <- modelError(model, testSet)

while (length(features) > 1) {
  # create a vector of all the RMSE's for the case when `i` predictors are considered
  tempErrors <- rep(0, length(features))  
  for (i in 1:length(features)){
    newFeatures <- features[features != features[i]]
    formula <- makeFormula(newFeatures)
    tempModel <- train(formula, 
                       data = trainingSet, 
                       method = "lm",
                       trControl = trainControl(method = "none")
                 )
    tempErrors[i] = modelError(tempModel, testSet)
  }
  
  # eliminate the predictor for which, when omitted, the method returned
  # the smallest RMSE
  optimalErrorIndex <- which.min(tempErrors)
  optErrors[length(features) - 1] <- tempErrors[optimalErrorIndex]
  toThrowAway <- features[optimalErrorIndex]
  features <- features[features != toThrowAway]
  print(sprintf("We threw away %s", toThrowAway))
}
print(sprintf("We kept %s", paste(features, collapse = ", ")))

################################################################################

# plot `error(number_of_features)`
plot(1:(length(data) - 1), optErrors, 
     xlab = "number of predictors",
     ylab = "RMSE"
)

print("features sorted by magnitude of their corresponding coefficient:")
betas <- model$finalModel$coefficients[-1]
names <- colnames(data)
sortedNames <- names[order(-abs(betas))]
print(sprintf("%s", paste(sortedNames, collapse = ", ")))

