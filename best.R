## Function best() returns the hospital in a given state which has
## the lowest morality rate for a given outcome. In case of a tie,
## the hospital which comes first alphabetically will be returned.
## The read columns are: [2] hospital.name, [7] state, 
##[11] MR_Heart Attack, [17] MR_Heart Failure, [23] MR_Pneumonia
best <- function(state, outcome) {
        file_name <- "outcome-of-care-measures.csv"
        rawdata <- read.csv(file_name, colClasses = "character")[,c(2,7,11,17,23)]
        colnames(rawdata) <-(c("hospital", "state", "heart attack", "heart failure",
                           "pneumonia"))
        if (sum(grepl(state, rawdata$state, ignore.case = TRUE)) == 0) {
                stop('invalid state')
        }
        else if (sum(grepl(outcome, colnames(rawdata),
                           ignore.case = TRUE)) == 0) {
                stop('invalid outcome')
        }
        
        data <- rawdata[,c("hospital", "state", outcome)]
        data
        bystate <- data[grep(state, data$state, ignore.case = TRUE),]
#         bystate[,outcome] <- suppressWarnings(as.numeric(levels(data[,outcome])[data[,outcome]]))
        orderdata <- bystate[order(bystate[,outcome]),]
        orderdata <- orderdata[complete.cases(orderdata),]
        return(orderdata[1,])
}

