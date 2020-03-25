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
        print(paste("k = ", as.character(i), sep = ""))
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
    print(errors)
    
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