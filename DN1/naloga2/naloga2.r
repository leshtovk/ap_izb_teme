#####################################
# load the libraries you need
#####################################
library(caret)
# library(caTools)

#########################################################################
# Ignore the warning
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################
RNGkind(sample.kind = "Rounding")

#####################################
# Load the necessary functions
#####################################
# setwd("~/repos/itap/DN1/naloga2")

problem = 2
source(sprintf("functions%d.r", problem))

####################################
# Solve the problem
####################################

data2 <- read.csv("podatki2.csv")

################################################################################
# 2.1 Basics
################################################################################

lin_model <- train(
    y ~., 
    data = data2, 
    method = "lm"
)

err_lm <- modelError(model = lin_model, data = data2, response = data2$y)
print(paste("Linear model error = ", err_lm, sep = ""))

################################################################################
# 2.2 Relevant Features
################################################################################

e_k0 <- find_k0(init_model = lin_model, data = data2, response = data2$y)
e_k0_val <- unname(e_k0)
print(paste("e_k0 = ", e_k0_val, sep = ""))

################################################################################
# 2.3 Additional Features 
################################################################################

names <- colnames(data2)
names <- names[-length(names)]
quad_formula <- makeQuadFormula(names)

quad_model <- train(
    quad_formula,
    data = data2, 
    method = "lm"
)

err_qm <- modelError(model = quad_model, data = data2, response = data2$y)
print(paste("Quadratic model error = ", err_qm, sep = ""))

################################################################################
# 2.4 Regularization 
################################################################################

beta_estimate <- ridgeCoefficients(data = data2, lambda = 0.01)
alpha <- squaredNorm(beta_estimate)
print(paste("alpha = ", alpha, sep = ""))

################################################################################
# 2.5 Magnitude of `lambda` 
################################################################################

g <- function(l) beta_norm(data = data2, lambda = l)

# lambdas <- seq(2000.0, 2200.0, length = 1000)
# g_lambdas <- betas(data2, lambdas)
# plot(lambdas, g_lambdas)

J1 <- leftStart(g, phi = alpha/10)
J2 <- J1 + 1

lambda_0_estimate <- bisectionSearch(g, J1, J2, alpha/10, 1*10^(-12))
print(paste("Estimate for lambda_0 = ", lambda_0_estimate, sep = ""))

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