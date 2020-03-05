library(caret)

x <- rnorm(11000)
M <- matrix(x, 1000, 11)

M.df <- as.data.frame(M)
colnames(M.df) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'Y')

indeks_ucna <- createDataPartition(M.df$Y, p = 4/5, list = FALSE)
ucna <- M.df[indeks_ucna, ]
testna <- M.df[-indeks_ucna, ]

natrenirano <- train(Y ~ ., data = ucna, method = "lm")
#pomembnost <- varImp(natrenirano, scale = FALSE)
