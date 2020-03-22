#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(caTools)

# nalozi jih tukaj, ne po klicu RNGkind spodaj
# load them here and not after the call of the RNGkind method below


#########################################################################
# Ignoriraj opozorilo (ignore the warning)
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################
RNGkind(sample.kind = "Rounding")

#####################################
# Nekaj testov
# Some tests
#####################################
# test_runif()
# test_sample()


#####################################
# Nalozi se potrebne funkcije
# Load the necessary functions
#####################################
# setwd("pot do mape (path to directory)")

# naloga_problem = 1
# source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

# vasa koda gre semkaj
# your code goes here

data1 <- read.csv("podatki1.csv")
data1$y <- as.factor(data1$y)

################################################################################
# 1.1 True Positives
################################################################################

phi = 0.6
truePositives <- sum((data1$y == 1) & (data1$z > phi))

################################################################################
# 1.2 Area Under the ROC Curve
################################################################################

auc <- unname(colAUC(data1$z, data1$y, plotROC = FALSE)[1, 1])
# `colAUC` is from `caTools`
# search for an alternative method to compute AUC

################################################################################
# 1.3 Important Feature
################################################################################

################################################################################
# 1.4 Modify the Data 
################################################################################

findLevels <- function(col1, col2) {
  lvs1 <- levels(col1)
  lvs2 <- levels(col2)
  
  lvs.frame <- expand.grid(x = lvs1, y = lvs2)
  d.lvs <- dim(lvs.frame)
  
  # `expand.grid` discovered from: 
  # https://stackoverflow.com/questions/4309217/cartesian-product-data-frame
  
  lvs <- rep("", times = d.lvs[1])
  for (i in 1:d.lvs[1]){
    coord1 <- as.character(lvs.frame[i, 1])
    coord2 <- as.character(lvs.frame[i, 2])
    lvs[i] <- paste("(", coord1, ", ", coord2, ")", sep = "")
  }
  
  return(lvs)
}

createNewTarget <- function(data, col1, col2){
  d.data <- dim(data)
  y1 <- rep("", times = d.data[1])
  for (i in 1:d.data[1]){
    coord1 <- as.character(col1[i])
    coord2 <- as.character(col2[i])
    y1[i] <- paste("(", coord1, ", ", coord2, ")", sep = "")
  }
  
  lvs <- findLevels(col1, col2)
  y1 <- factor(y1, levels = lvs, ordered = FALSE)
}

y1 <- createNewTarget(data1, data1$y, data1$x6)
data1$y1 <- y1

countGroups <- function(col, lvs){
  len.lvs <- length(lvs)
  counter <- rep(0, times = len.lvs)
  for (i in 1:len.lvs){
    counter[i] <- sum(col == lvs[i])
  }
  
  return(counter)
}

lvs <- levels(y1)
group_counter <- countGroups(y1, lvs)
smallest_group <- lvs[which.min(group_counter)]

################################################################################
# 1.5 Final Model 
################################################################################




###############################################
# Kode pod tem ne spreminjaj
# Do not change the code below
###############################################

test_runif = function(){
 set.seed(1234)
 x = runif(5);
 x1 = c(0.1137034113053232, 0.6222994048148394, 0.6092747328802943, 0.6233794416766614, 0.8609153835568577)
 if (sum(abs(x - x1)) > 10^-10){
  stop("Test runif ni ok (has failed)")
 }
 print("runif works fine")
}

test_sample = function(){
 set.seed(1234)
 x = sample(20);
 x1 = c(3, 12, 11, 18, 14, 10, 1, 4, 8, 6, 7, 5, 20, 15, 2, 9, 17, 16, 19, 13)
 if (sum(abs(x - x1)) > 0){
  stop("Test sample ni ok (has failed)")
 }
 print("sample works fine")
}

test_runif()
test_sample()