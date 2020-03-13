library(MASS)
# fix(Boston)
attach(Boston)

# fit the model 
# medv = b0 + (b1 * lstat) + (b2 * age)
lm.fit <- lm(medv ~ lstat + age)

# summary(lm.fit)
# notice the p-statistic for the coefficient of `age`

coefs <- coef(lm.fit)
b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]

# plot how `medv` changes as `age` rises
plot(age, medv, main = "medv vs age")
abline(median(medv), 0, lwd = 3, col = "red")

# see how the the amount of data points 
hist(age)

f <- function (x, y) b0 + (b1 * x) + (b2 * y)
x <- seq(0, 100, length = 100)
y <- seq(0, 100, length = 100)
Z <- outer(x, y, f)

# plot a graph of the model
persp(x, y, Z, theta = 45, 
      xlab = "lstat", ylab = "age", zlab = "medv",
      main = "Graph of the model")