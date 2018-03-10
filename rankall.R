rankall <- function(outcome, num = "best") {
	setwd("C:/Users/jessica/Coursework/ProgrammingAssignment3")
	outc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	outc[, 11] <- as.numeric(outc[, 11])
	outc[, 17] <- as.numeric(outc[, 17])
	outc[, 23] <- as.numeric(outc[, 23])

	##Check whether the outcome is valid and if not, stop the function
	if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
		stop("invalid outcome")
	}

	outcome1 <- outc[, c(2, 7, 11, 17, 23)]

	if(outcome == "heart attack") {
				data <- outcome1[!is.na(outcome1[, 3]),]
				outcome2 <- data[order(data[, 2], data[, 3], data[, 1]),]
			} else if(outcome == "heart failure") {
				data <- outcome1[!is.na(outcome1[, 4]),]
				outcome2 <- data[order(data[, 2], data[, 4], data[, 1]),]
			} else { 
				data <- outcome1[!is.na(outcome1[, 5]),]
				outcome2 <- data[order(data[, 2], data[, 5], data[, 1]),]
			}
	
	statedata <- split(outcome2, outcome2$State)
	
	if(num == "best") {
		final <- sapply(statedata, function(x) x[1, 1:2])
	} else if(num == "worst") {
		final <- sapply(statedata, function(x) tail(x[, 1:2], 1))
	} else {
		final <- sapply(statedata, function(x) x[num, 1:2])
	}
		
	final <- as.data.frame(final)
	final2 <- as.data.frame(t(final))
	final2
}