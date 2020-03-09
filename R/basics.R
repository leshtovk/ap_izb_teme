# navigating different direcotries in the terminal doesn't change the 
# current working directory of Rstudio
# see the working direcotry with `getwd()`
# change the working directory with `setwd()`
# start editing a script with `file.edit('filename.R')`
# if `filename.R` doesn't exist in the wd, it will be created

#############################################################

# vectors

a <- c(1, 2, 3, 4, 5) 
b <- c(5, 6, 6, 7, 8)

c <- a + b
eqlen <- length(a) == length(b)
dima <- dim(a)  

d <- 1:4
e <- -1:-4
f <- d + e
fzeros <- f == 0

g = c(1:3, 4, 3:1)
h <- seq(from=10, to=-8, by=-2)
i <- c(seq(from=1, to=1, length=3), seq(from=2, to=2, length=length(h)- 3))
j <- rep(1, 100)

corresp <- i[h > 0]

####################################################

# matrices

A <- matrix(1:16, 4, 4)
B <- A[seq(from=2, to=4, by=2), seq(from=1, to=3, by=2)]
C <- matrix(rep(1, nrow(A), 4, 4)

D <- A[seq(from=1, to=4, length=2),]
E <- A[, 3:2]
F <- A[c(1, 3), c(2, 3)]

G <- A[, -1]
H <- A[-c(2, 3),]
I <- F[, -c(2, 3)]

x <- seq(from=10, to=-10, by=-2)
dimx <- dim(x)

rx <- matrix(x, 1, length(x))
dimrx <- dim(rx)

cx <- matrix(x, length(x), 1)
dimcx <- dim(cx)

makesure <- dimrx[2] == dimcx[1]

#######################################################

# get `n` random, normally distributed numbers:

n <- 16
q <- rnorm(n)
Q <- matrix(q, 4, 4)

#######################################################

# plotting

plt <- function(lx, ly)plot(lx, ly)

# use additional parameters to display information: 
#   `xlab` = 'x'
#   `ylab` = 'y'
#   `main` = 'title'

# use `pdf(title.pdf)` to save the plot to a pdf
# use `jpeg(title.jpg)` to save the plot as a jpeg
# use `dev.off` to tell R that you are done creating a plot


p1x <- rnorm(1000)
p1y <- rnorm(1000)

t2 <- seq(from=0, to=2*pi, length=5000)
p2x <- cos(t2)
p2y <- sin(t2)

# 3D plotting

prp = function(lx, ly, Z, th, ph){
  persp(lx, ly, Z, theta = th, phi = ph)
}

p3y <- p3x <- seq(from=-1, to=1, length=20)
cone <- function(x, y)sqrt(x^2 + y^2)
Z3 <- outer(p3x, p3y, cone)

# divide the display window 
par(mfrow = c(2, 2)