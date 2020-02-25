college <- read.csv('College.csv')
attach(college)

# assign the names of the universities as the names of the rows 
rownames(college) <- college[, 1]

# remove the column with the names of the universities 
college <- college[, 2:dim(college)[2]]

# alternatively, use: college <- college[, -1]
# the `row.names` column is not a data column
# the first real column of the dataset is `Private`
# the names of the universities are now accessible through 
# `rownames(college)`; `colnames(college)` does the same for columns

# classify the universities as 'elite' or 'not elite'
elite_inds <- (1:dim(college)[1])[Top10perc > 50]
Elite <- rep('No', nrow(college))
Elite[elite_inds] <- 'Yes'
Elite <- as.factor(Elite)

# create a new column in the table
college <- data.frame(college, Elite)

# create a separate tables of only the elite and the rest of the universities
# in these tables the `Elite` variable will not be needed
college.elite <- college[Elite == 'Yes', ]
college.elite <- college.elite[, -dim(college.elite)[2]]

college.common <- college[Elite == 'No', ]
college.common <- college.common[, -dim(college.common)[2]]

edim <- dim(college.elite)
cdim <- dim(college.common)

# let's get the averages of every variable in the two tables 
# and save the results as dataframes
el.avgs <- c()
cm.avgs <- c()

for (i in 2:edim[2]){ 
  # notice that `Private` will not be included
  el.avg <- sum(college.elite[, i]) / edim[1]
  cm.avg <- sum(college.common[, i]) / cdim[1]
  
  el.avgs <- c(el.avgs, el.avg)  
  cm.avgs <- c(cm.avgs, cm.avg)
}

el.avgs <- matrix(el.avgs, 1, edim[2] - 1)
cm.avgs <- matrix(cm.avgs, 1, cdim[2] - 1)
# -1 to account for the lack of `Private`

# we need to turn the matrices into dataframes and name the variables
el.avgs <- as.data.frame(el.avgs)
cm.avgs <- as.data.frame(cm.avgs)

vars <- colnames(college)
colnames(el.avgs) <- colnames(cm.avgs) <- vars[2:(dim(college)[2] - 1)]

# write the two dataframes to the disk as `.csv` files
# write.csv(el.avgs, 'elite.averages.csv')
# write.csv(cm.avgs, 'common.averages.csv')

# finally, compare the averages
I <- rep(1, length=17)
res <- sum(I[el.avgs[1, ] > cm.avgs[1, ]])
common.wins = colnames(cm.avgs)[cm.avgs[1, ] > el.avgs[1, ]]