library(MASS)
library(ISLR)

attach(Boston)

# train a linear model 
# regressing `medv` over `lstat`
lm.fit <- lm(medv ~ lstat)

#confint(lm.fit)
#predict(lm.fit, data.frame(lstat = c(0, 5, 10, 15)), 
#        interval = "confidence", level = 0.95)

# divide the plotting window into two parts
par(mfrow = c(1, 2))
pdf("meanv_over_lstat.pdf")

# plot the results of the regression 
plot(lstat, medv, xlab = "lstat", ylab = "medv",
     main = "Regressing the median value over low status",
     xlim = c(0, 50), ylim = c(0, 50))
abline(lm.fit, lwd = 3, col = "red")

# plot the residuals versus the predictions 
lm.pred <- predict(lm.fit)
lm.res <- residuals(lm.fit)
plot(lm.pred, lm.res, 
     xlim = c(-10, 35), ylim = c(-10, 35),
     main = "Predictions over residuals")
dev.off()

# there is a pattern in the residuals, 
# suggesting that there is a non-linear relationship