Auto2 <- read.csv("Auto.csv", header = TRUE, na.string = "?")
#fix(Auto2)

# since only 5 rows contain missing data, we can remove them
Auto2 = na.omit(Auto2)

dimAuto2 = dim(Auto2)
nmsAuto2 = names(Auto2)

# make the variables in the data available by name
# alternatively, use for example Auto2$mpg
attach(Auto2)

p = function(lx, ly)plot(lx, ly)
