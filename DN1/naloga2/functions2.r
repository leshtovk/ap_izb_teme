error <- function(y, y_pred){
    sqrt(mean((y - y_pred)^2))
}

model_rmse <- function(model, data, response){
    predictions <- predict(model, newdata = data)
    error(response, predictions)
}

makeQuadFormula <- function(names) {
    names_len <- length(names)
    names_extended <- c(names, rep("", times = names_len))
    
    for (i in 1:names_len){
       names_extended[names_len + i] <- paste("I(", names[i], "^2)", sep = "")  
    }
    as.formula(paste("y ~", paste(names_extended, collapse = "+")))
}