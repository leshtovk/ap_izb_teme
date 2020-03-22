library(caret)

################################################################################
# Implement `createData` and `extendData`
################################################################################

nameColumn <- function(i){
  sprintf("X%d", i)
}

createData <- function(n, p, seed = 123) {
  # `n` is the number of observations 
  # `p` is the number of predictors
  # 
  # the response variable: 
  # `y = linear_factors + sines + mixed_factors + noise`
  
  xs <- matrix(runif(n * p), ncol = p)
  
  # "pseudorandomness" for each step of creating the response variable 
    # example: `linear_factors = r1x1 + ... rpxp` instead of `x1 + ... + xp`
    # `r1, ..., rp` are pseudorandom numbers between 0 and 1 
  alpha <- runif(p)
  beta <- runif(p)
  gamma <- runif(p - 1)
  
  # create mixed factors
  mixedFactors <- matrix(rep(0.0, times = n * (p - 1)), ncol = p - 1)
  
  for (i in 2:p) {
    mixedFactors[, i - 1] <- sqrt(xs[, i - 1] * xs[, i])   
  }
  
  # create response variable
  
  # linear factors 
  Y <- xs %*% alpha
  # add sines
  Y <- Y + sin(xs) %*% beta
  # add mixed factors 
  Y <- Y + mixedFactors %*% gamma
  # add noise
  Y <- Y + rnorm(n, sd = 0.01) 
  
  # create a list of the names of the predictors 
  columnNames <- lapply(1:(p + 1), nameColumn)
  columnNames[p + 1] <- "Y"
  
  # create a data frame 
  data <- as.data.frame(xs)
  data$Y <- Y
  colnames(data) <- columnNames
  
  return(data)
}

extendData <- function(data, order) {
  if (order == 1){
    return(data)
  }
  
  d = dim(data)
  p <- d[2] - 1   # number of predictors  
  
  extendedData <- data.frame(
                    poly(as.matrix(data[, 1:p]), degree = order, raw = TRUE), 
                    Y = data$Y
                  ) 
}

################################################################################
# Order 2
################################################################################

n <- 5000
p <- 3
k <- 10

# create the data sets

data <- createData(n, p)
dataord2 <- extendData(data, order = 2)

# train linear models 

tc <- trainControl(method = "cv", number = k, seeds = 1:(k + 1))

lin <- train(
  Y ~., data = data, trControl = tc, method = "lm"
)

lin2 <- train(
  Y ~., data = dataord2, trControl = tc, method = "lm"
)

# compare errors, estimated with CV

errors <- c(lin$results$RMSE, lin2$results$RMSE)

print(
  sprintf("Estimated RMSE: %s", paste(errors, collapse = ", "))
)
print(
  sprintf("Model %d is better", which(errors == min(errors)))
)  

################################################################################
# Higher orders
################################################################################

maxOrder <- 10 

errors <- c(errors, rep(0.0, times = maxOrder - 2))

# create a list of training RMSE's 
errorsTrain <- rep(0.0, times = maxOrder)
errorsTrain[1] <- RMSE(predict(lin, newdata = data), data$Y)
errorsTrain[2] <- RMSE(predict(lin2, newdata = dataord2), data$Y)

# construct the models and save the errors 
i <- 3
while(i <= maxOrder){
  data_i <- extendData(data, order = i)
  print(sprintf("Created data set of order %d", i))
  
  lin_i <- train(Y ~., data = data_i, trControl = tc, method = "lm")
  print(sprintf("Constructed model %d", i))
  
  errors[i] <- lin_i$results$RMSE
  errorsTrain[i] <- RMSE(predict(lin_i, newdata = data_i), data$Y)
  i <- i + 1
}

par(mfrow = c(1, 2))

# plot how the estimated RMSE changes with order 
plot(1:maxOrder, errors, 
     xlab = "order of data extension", 
     ylab = "RMSE",
     type = "o", col = "blue")

# add how the test RMSE changes with order 
lines(1:maxOrder, errorsTrain, 
     xlab = "order of data extension", 
     ylab = "RMSE", 
     type = "o", col = "red")

# add a legend to the plot
legend(x = "topright", 
       legend = c("cross validation", "training set"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 1)

################################################################################
# KNN
################################################################################

n <- 500
p <- 3
data <- createData(n, p)

ks <- 1:20
errorsTrain <- rep(0.0, times = length(ks))

tc <- trainControl(
  method = "cv", number = k, 
  index = createFolds(data$Y, k = k, returnTrain = TRUE)
)

knn <- train(
  Y ~., 
  data = data, 
  trControl = tc, 
  method = "knn",
  tuneGrid = data.frame(k = ks)
)

# estimated errors
errors <- knn$resilts$RMSE

# training errors 
for (i in 1:length(ks)){
  knn <- train(
    Y ~., 
    data = data,
    trControl = tc,
    method = "knn", 
    tuneGrid = data.frame(k = ks[i])
  )
  
  errorsTrain[i] <- RMSE(predict(knn, newdata = data), data$Y)
}

# plot how the estimated RMSE changes with `k`
plot(ks, errors, 
     xlab = "k", ylab = "RMSE",
     type = "o", col = "blue" 
)

# plot how the training set RMSE changes with `k`
lines(ks, errorsTrain,
      xlab = "k", ylab = "RMSE",
      type = "o", col = "red"
)

legend(x = "bottomright",
       legend = c("cross validation", "training set"),
       col = c("blue", "red"),
       lty = 1, lwd = 1
)
