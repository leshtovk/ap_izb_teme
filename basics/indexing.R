A <- matrix(1:16, 4, 4)
B <- A[seq(from=2, to=4, by=2), seq(from=1, to=3, by=2)]

C <- A[seq(from=1, to=4, length=2),]
D <- A[, 3:2]

E <- A[, -1]
F <- A[-c(2, 3),]
G <- F[, -c(2, 3)]

a <- seq(from=10, to=-10, by=-2)
dima <- dim(a)

ra <- matrix(a, 1, length(a))
dimra <- dim(ra)

ca <- matrix(a, length(a), 1)
dimca <- dim(ca)

makesure <- dimra[2] == dimca[1]