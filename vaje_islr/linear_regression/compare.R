library(MASS)
# fix Boston
attach(Boston)

lm.fit1 <- lm(medv ~ lstat, data = Boston)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)

# analysis of variance table: anova(lm.fit1, lm.fit2)
## null - the two models fit the data equally well 
## alternative - the full model is better
## notice the F-statistic and its associated p-value

## compare the patterns that the residuals form 
# par(mfrow = c(2, 2))
# plot(lm.fit1)
# plot(lm.fit2)
