x0 = 0.1
r.range1 <- seq(from = 0, to = 4, length = 401)
r.range2 <- seq(from = 3, to = 4, length = 501)
r.range3 <- seq(from = 3.738, to = 3.74, length = 401)
x1 <- rep(0, times = 300)
x2 <- rep(0, times = 500)
x3 <- rep(0, times = 1000)
# any value for `x0`` in `seq(from = 0.1, to = 0.9, length = 9)`
# will generate the same result

f <- function(x, x0, r) {
  x[1] = x0
  for (i in 2:length(x)) {
   x[i] = r * x[i-1] * (1 - x[i-1]) 
  }
  return(x)
}

F <- function(x, x0, r.range, f, pdf.title) {
  pdf(pdf.title)
  for (j in 1:length(r.range)) {
    title <- paste("x0 = ", x0, ", ", "r = ", r.range[j])
    plot(1:length(x), f(x, x0, r.range[j]), main = title)
  }
  dev.off()
}

