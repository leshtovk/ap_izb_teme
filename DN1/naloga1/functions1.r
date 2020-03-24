#########################
# For section 1.3 
#########################

makePlots <- function(data, feature_names, response_names){
    len_feat <- length(feature_names)
    len_resp <- length(response_names)
    
    data_name <- deparse(substitute(data))
    pdf("problem1_3.pdf")
    
    # `deparse(substitute(obj))` trick discovered from:
    # paste(
    #     "https://stackoverflow.com/questions/10520772/",
    #     "in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function",
    #     sep = ""
    # )
    
    for (i in 1:len_resp){
        resp_name <- response_names[i]
        resp <- data[[resp_name]]
        
        for (j in 1:len_feat){
            feat_name <- feature_names[j]
            feat <- data[[feat_name]]

            plot_label <- paste("data = ", data_name, sep = "")
            plot(
                feat, resp,
                xlab = feat_name, ylab = resp_name, 
                main = plot_label
            )
        }
    }
    dev.off()
}

#########################
# For section 1.4 
#########################

findLevels <- function(col1, col2) {
    levels1 <- levels(col1)
    levels2 <- levels(col2)
    
    levels_frame <- expand.grid(x = levels1, y = levels2)
    levels_dim <- dim(levels_frame)
    
    # `expand.grid` discovered from: 
    # https://stackoverflow.com/questions/4309217/cartesian-product-data-frame
    
    levels <- rep("", times = levels_dim[1])
    for (i in 1:levels_dim[1]){
        coord1 <- as.character(levels_frame[i, 1])
        coord2 <- as.character(levels_frame[i, 2])
        levels[i] <- paste("(", coord1, ", ", coord2, ")", sep = "")
    }
    
    return(levels)
}


createNewTarget <- function(data, col1, col2){
    data_dim <- dim(data)
    y1 <- rep("", times = data_dim[1])
    for (i in 1:data_dim[1]){
        coord1 <- as.character(col1[i])
        coord2 <- as.character(col2[i])
        y1[i] <- paste("(", coord1, ", ", coord2, ")", sep = "")
    }
    
    lvs <- findLevels(col1, col2)
    y1 <- factor(y1, levels = lvs, ordered = FALSE)
}


countGroups <- function(col, levels){
    levels_len <- length(levels)
    counter <- rep(0, times = levels_len)
    for (i in 1:levels_len){
        counter[i] <- sum(col == levels[i])
    }
    
    return(counter)
}

#########################
# For section 1.5 
#########################

microRecall <- function(classes, y, y_predictions){
    n <- length(y)
    p <- 0
    tp <- 0
    
    for (a in classes){
        p <- p + sum(y == a)
        tp <- tp + sum(y == a & y_predictions == a)
    }
    
    return(tp/p)
}

