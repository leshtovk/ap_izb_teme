#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
# library(caTools)

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

naloga_problem = 2
source(sprintf("functions%d.r", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

data2 <- read.csv("podatki2.csv")

################################################################################
# 2.1 Basics
################################################################################

lin_model <- train(
    y ~., 
    data = data2, 
    method = "lm",
    trControl = trainControl(verboseIter = TRUE)
)

lin_predictions <- predict(
    lin_model, 
    newdata = data2, 
    type = "raw")

err_lm <- model_rmse(model = lin_model, data = data2, response = data2$y)

################################################################################
# 2.2 Relevant Features
################################################################################

################################################################################
# 2.3 Additional Features 
################################################################################

names <- colnames(data2)
names <- names[-length(names)]
quad_formula <- makeQuadFormula(names)

quad_model <- train(
    quad_formula,
    data = data2, 
    method = "lm",
    trControl = trainControl(verboseIter = TRUE)
)

err_qm <- model_rmse(model = quad_model, data = data2, response = data2$y)

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