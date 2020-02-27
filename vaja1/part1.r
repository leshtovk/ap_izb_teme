library(datasets)
library(caret)

data("iris")
trainIndex <- createDataPartition(y = iris$Species, p = 0.8, 
                                  list = FALSE, times = 1)

# `trainIndex` is a matrix with a row of indeces from 1 to 150 
# (dim1 of `trainIndex`) / (amount of indeces not in `trainIndex`) = 4,
# because of `p = 0.8`

# `y = iris$Species` means that the data will be isplit relative to `Species`
# the overall distribution of the data relative to `Species` is preserved 

# `list = FALSE` makes sure that `trainIndex` is not a list
# vectors can be passed as subscripts, not lists

# make two new data frames: 
irisTrain <- iris[trainIndex, ]
irisTest <- iris[-trainIndex, ]


## to use the `train` functio we need at last 3 things: 
# a data frame where samples are in rows and features are in columns 
# a vector containing the outcome for each sample 
# a model 
