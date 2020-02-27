library(caret)
data(iris)

## split data 
#
#indeks_ucna = createDataPartition(iris$Species, p = 4/5, list = FALSE)
#ucna = iris[indeks_ucna, ]
#testna = iris[-indeks_ucna, ]
#
## train 
#
##natrenirano = train(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, 
##                    data = ucna, method = "knn")
#
## fix `k` to a constant value 
##natrenirano = train(Species ~ ., data = ucna, method = "knn", 
##                    tuneGrid = data.frame(k = 1))
#
## state which values of `k` should be tested for 
#natrenirano = train(Species ~ ., data = ucna, method = "knn", 
#                    tuneGrid = data.frame(k = 1:20))
#
##plot(natrenirano)
#
#napovedi = predict(natrenirano, newdata = testna)
##n_pravilnih = sum(napovedi == testna$Species)
##print(sprintf("Natancnost modela na testni: %.3f", n_pravilnih / length(napovedi)))

# ----------------------------------------------------------------------------------------

podatki <- read.csv("podatki.csv" ,fileEncoding = "utf-8")
podatki$X4 <- gsub('“|”', '', as.character((podatki$X4)))
podatki$X6 <- gsub('“|”', '', as.character((podatki$X6)))
podatki$X4 <- gsub('"|"', '', as.character((podatki$X4)))
podatki$X6 <- gsub('"|"', '', as.character((podatki$X6)))

podatki$X4[podatki$X4 == 'YES' | podatki$X4 == 'DA'] <- '1'
podatki$X4[podatki$X4 == 'NO' | podatki$X4 == 'NE'] <- '0'

podatki$X4 <- as.numeric(as.character(podatki$X4))
podatki$X6 <- as.numeric(as.character(podatki$X6))
podatki$Y <- as.factor(as.numeric(podatki$Y))

indeksi_ucna2 <- createDataPartition(podatki$Y, p = 4/5, list = FALSE)
ucna2 <- podatki[indeksi_ucna2, ]
testna2 <- podatki[-indeksi_ucna2, ]

natrenirano2 <- train(Y ~ . -X, 
                      data = ucna2, method = "knn")

natrenirano3 <- train(Y ~ . -X, 
                      data = ucna2, method = "rf")


#natrenirano22 <- train(Y ~ . -X, 
#                      data = ucna2, method = "knn", 
#                      tuneGrid = data.frame(k = 1:30))