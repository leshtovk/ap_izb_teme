#########################
# Lifted from `Vaja2`
#########################

error <- function(y, y_pred){
    sqrt(mean((y - y_pred)^2))
}

modelError <- function(model, data, response){
    predictions <- predict(model, newdata = data)
    error(response, predictions)
}

makeFormula <- function(names) {
    as.formula(paste("y ~", paste(names, collapse = "+")))
}

#########################
# For section 2.2
#########################

relevantFeatures <- function(coefs, k){
    coefs <- rev(coefs)
    while(length(coefs) > k){
        least_relevant <- coefs[which.min(abs(coefs))]
        coefs <- coefs[coefs != least_relevant]
    }
    return(names(rev(coefs)))
}


find_k0 <- function(init_model, data, response){
    coefs <- init_model$finalModel$coefficients[-1]
    p <- length(coefs)
    
    errors <- rep(0.0, times = p)
    errors[p] <- modelError(model = init_model, data, response)
    
    print("computing values: e_k")
    for (i in 1:(p-1)){
        i_most_relevant <- relevantFeatures(coefs, k = i)
        formula <- makeFormula(i_most_relevant)
        temp_model <- train(
            formula, 
            data = data,
            method = "lm"
        )
        data_i <- data[i_most_relevant]
        errors[i] <- modelError(model = temp_model, data = data_i, response)
    }

    err_names <- rep("", times = p)
    for (j in 1:p){
        err_names[j] <- paste("e_", as.character(j), sep = "")
    }
    
    names(errors) <- err_names
    ref <- unname(errors[p] * 1.1)
    e_k0 <- errors[which(errors < ref)[1]]
}

#########################
# For section 2.3
#########################

makeQuadFormula <- function(names) {
    names_len <- length(names)
    names_extended <- c(names, rep("", times = names_len))
    
    for (i in 1:names_len){
        names_extended[names_len + i] <- paste("I(", names[i], "^2)", sep = "")  
    }
    as.formula(paste("y ~", paste(names_extended, collapse = "+")))
}

#########################
# For section 2.4
#########################

ridgeCoefficients <- function(data, lambda){
    data_dim <- dim(data)
    p <- data_dim[2] - 1
    
    D <- unname(as.matrix(data))
    X <- D[, 1:p]
    X_t <- t(X)
    y <- D[, p+1]

    A <- (X_t %*% X) + (lambda * diag(p))
    b <- X_t %*% y
    
    beta <- solve(A, b)
}

squaredNorm <- function(vect) { sum(vect^2) }

#########################
# For section 2.5
#########################

beta_norm <- function(data, lambda){
    estimate <- ridgeCoefficients(data, lambda)
    return(squaredNorm(estimate))
}

betas <- function(data, lambdas){
    l <- length(lambdas)
    sqnorms <- rep(0.0, times = l)
    for (i in 1:l){
        sqnorms[i] <- beta_norm(data, lambda = lambdas[i])
    }
    return(sqnorms)
}

leftStart <- function(f, phi){
    x <- 0
    while (f(x) > phi){
        x <- x + 1
    }
    return(x - 1)
}

bisectionSearch <- function(f, left_start, right_start, phi, tol){
    est1 <- left_start
    est2 <- right_start
    
    while (abs(est1 - est2) >= tol){
        s <- (est1 + est2)/2
        if (f(s) > phi){
            est1 <- s
        }
        else {
            est2 <- s
        }
    }
    return(est1)
}




