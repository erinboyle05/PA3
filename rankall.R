## Function rankall() sorts the 
## The read columns are: [2] hospital.name, [7] state, 
##[11] MR_Heart Attack, [17] MR_Heart Failure, [23] MR_Pneumonia
rankall <- function(outcome, num = "best") {
        file_name <- "outcome-of-care-measures.csv"
        
        if (outcome == "heart attack") {
              rawdata <- read.csv(file_name, colClasses = "character")[,c(2,7,11)]
              names(rawdata) <- c("hospital", "state", "heart attack")
              rawdata[,2] <- as.factor(rawdata[,2])
              rawdata[,3] <- suppressWarnings(as.numeric(rawdata[,3]))
        }
        else if (outcome == "heart failure") {
                rawdata <- read.csv(file_name, colClasses = "character")[,c(2,7,17)]
                names(rawdata) <- c("hospital", "state", "heart failure")
                rawdata[,2] <- as.factor(rawdata[,2])
                rawdata[,3] <- suppressWarnings(as.numeric(rawdata[,3]))
        }
        else if (outcome == "pneumonia") {
                rawdata <- read.csv(file_name, colClasses = "character")[,c(2,7,23)]
                names(rawdata) <- c("hospital", "state", "pneumonia")
                rawdata[,2] <- as.factor(rawdata[,2])
                rawdata[,3] <- suppressWarnings(as.numeric(rawdata[,3]))
        }
        else {
                stop('invalid outcome')
        }
        states <- levels(rawdata[,2])
        output <- data.frame()
        for (i in 1:length(states)) {
                statedata <- rawdata[grep(states[i], rawdata[,2]),]
                if (num == "best") {
#                         num == 1
                        statedata <- statedata[complete.cases(statedata),]
                        orderdata <- statedata[order(statedata[,3],
                                        statedata[,1]),]
                        output <- rbind.data.frame(output, orderdata[1,1:2])
                        
                }
                else if (num == "worst") {
                        orderdata <- statedata[order(statedata[,3],
                                        statedata[,1],decreasing = TRUE),]
                        statedata <- statedata[complete.cases(statedata),]
                        output <- rbind.data.frame(output, orderdata[1,1:2])
                        
                }
                else {
                        statedata <- statedata[complete.cases(statedata),]
                        orderdata <- statedata[order(statedata[,3],
                                                     statedata[,1]),]
                        output <- rbind.data.frame(output, orderdata[num,1:2]) 
                }
                        

        }
#         output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
#         colnames(output) <- c("hospital", "state")
#         rownames(output) <- output[,2]
#         return(output)
        output
}