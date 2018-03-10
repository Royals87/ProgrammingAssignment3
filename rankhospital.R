rankhospital <- function(state, outcome, num = "best") {
	setwd("C:/Users/jessica/Coursework/ProgrammingAssignment3")
	statedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	statedata[, 11] <- as.numeric(statedata[, 11])
	statedata[, 17] <- as.numeric(statedata[, 17])
	statedata[, 23] <- as.numeric(statedata[, 23])	

	##Check whether the state input is valid
	if(!(state %in% statedata$State)) {
		stop("invalid state")
	}

	##Check whether the outcome is valid and if not, stop the function
	if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
		stop("invalid outcome")
	}

	stateoutc <- statedata[which(statedata$State == state), c(2, 11, 17, 23)]

	stateoutc2 <- if(outcome == "heart attack") {
				tapply(stateoutc[, 1], stateoutc[, 2], sort)
			} else if(outcome == "heart failure") {
				tapply(stateoutc[, 1], stateoutc[, 3], sort)
			} else { 
				tapply(stateoutc[, 1], stateoutc[, 4], sort)
			}
	final <- unlist(stateoutc2)

	if(num == "best") {
		num <- 1
	} else if(num == "worst") {
		num <- length(final)	
	}

	final[num]
}