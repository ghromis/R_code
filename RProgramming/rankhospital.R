rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("./ProgAssignment3/outcome-of-care-measures.csv", 
                   colClasses = "character")
  data[,11] <- suppressWarnings(as.numeric(data[,11]))
  data[,17] <- suppressWarnings(as.numeric(data[,17]))
  data[,23] <- suppressWarnings(as.numeric(data[,23]))
  ## Check that state and outcome are valid
  states <- unique(data$State)
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!state %in% states) {stop ("invalid state")}
  if (!outcome %in% outcomes){stop ("invalid outcome")}
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  pickState <- data[data$State == state,]
  if (outcome == "heart attack"){column <- 11}
  if (outcome == "heart failure"){column <- 17}
  if (outcome == "pneumonia"){column <- 23}
  
  if (num == "best"){target=1}
  else if (num == "worst"){target=length(na.omit(pickState[,column]))}
  else {target =num}
  
  sort <- pickState[order(pickState[,column],pickState[,2]),]
  return (sort[target,2])
}