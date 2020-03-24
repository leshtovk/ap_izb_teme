#####################################
# load the libraries you need
#####################################
library(caret)
library(caTools)

# load them here and not after the call of the RNGkind method below

#########################################################################
# Ignore the warning
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################

RNGkind(sample.kind = "Rounding")

#####################################
# Load the necessary functions
#####################################
# setwd("~/repos/itap/DN1/naloga1")

problem = 1
source(sprintf("functions%d.r", problem))

####################################
# Solve the problem
####################################

data1 <- read.csv("podatki1.csv")
data1$y <- as.factor(data1$y)

################################################################################
# 1.1 True Positives
################################################################################

phi = 0.6
true_positives <- sum((data1$y == 1) & (data1$z > phi))

################################################################################
# 1.2 Area Under the ROC Curve
################################################################################

auc <- unname(colAUC(data1$z, data1$y, plotROC = FALSE)[1, 1])

################################################################################
# 1.3 Relevant Feature
################################################################################

xs <- c("x1", "x2", "x3")
makePlots(data = data1, feature_names = xs, response_names = "y")

# from the plots we can see that `x2` is the predictor we are interested in
x2_lower <- data1$x2[data1$y == "0"]
x2_upper <- data1$x2[data1$y == "1"]
phi_0 <- max(x2_lower)
phi_1 <- min(x2_upper)
max_int_len <- phi_1 - phi_0

################################################################################
# 1.4 Modify the Data 
################################################################################

y1 <- createNewTarget(data = data1, col1 = data1$y, col2 = data1$x6)
data1$y1 <- y1

lvs <- levels(y1)
group_counter <- countGroups(col = y1, levels = lvs)
smallest_group <- lvs[which.min(group_counter)]

################################################################################
# 1.5 Final Model 
################################################################################

knn_model <- train(
  y1 ~ x1 + x2 + x3 + x4 + x5,
  data = data1,
  method = "knn",
  tuneGrid = data.frame(k = 1)
)

predictions <- predict(knn_model, newdata = data1)
knn_mr <- microRecall(
  classes  = lvs, 
  y = y1, 
  y_predictions = predictions
)

###############################################
# The code below has been changed by me
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