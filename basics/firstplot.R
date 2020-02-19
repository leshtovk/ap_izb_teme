x <- 1:10
y <- 11 : 20

a <- rnorm(1500)
b <- rnorm(1500)

jpeg('firstplot.jpg') 
plot(a, b, xlab = 'x-axis', ylab = 'y-axis', 
     main = 'Plot of random numbers')

dev.off() # indicates that we are done creating the plot
