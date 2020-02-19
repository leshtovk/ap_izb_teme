college <- read.csv('College.csv')

# assign the names of the universities as the names of the rows 
rownames(college) <- college[, 1]

# remove the column with the names of the universities 
college <- college[, 2:dim(college)[2]]

# alternatively, use: college <- college[, -1]
# the `row.names` column is not a data column
# the first real column of the dataset is `Private`
# the names of the universities are now accessible through `row.names(college)`

# extract a table of only elite universities
elite_inds <- (1:dim(college)[1])[Top10perc > 50]
Elites <- college[elite_inds, ]

# classify the colleges as 'elite' or 'not elite'
Elite <- rep('No', nrow(college))
Elite[elite_inds] <- 'Yes'
Elite <- as.factor(Elite)
