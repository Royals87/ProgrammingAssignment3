best <- function (state, outcome) {
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
				stateoutc[which.min(stateoutc[, 2]),]
			} else if(outcome == "heart failure") {
				stateoutc[which.min(stateoutc[, 3]),]
			} else { 
				stateoutc[which.min(stateoutc[, 4]),]
			}

	first <- sort(stateoutc2)
	first$Hospital.Name
}