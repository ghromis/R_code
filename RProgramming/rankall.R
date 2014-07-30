rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("./ProgAssignment3/outcome-of-care-measures.csv", 
                   colClasses = "character")
  data[,11] <- suppressWarnings(as.numeric(data[,11]))
  data[,17] <- suppressWarnings(as.numeric(data[,17]))
  data[,23] <- suppressWarnings(as.numeric(data[,23]))
  ## Check that state and outcome are valid
  states <- sort(unique(data$State))
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!outcome %in% outcomes){stop ("invalid outcome")}
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  if (outcome == "heart attack"){column <- 11}
  if (outcome == "heart failure"){column <- 17}
  if (outcome == "pneumonia"){column <- 23}  
  df <- data.frame()
  for (i in states){
    pickState <- data[data$State == i,]
    
    if (num == "best"){target=1}
    else if (num == "worst"){target=length(na.omit(pickState[,column]))}
    else {target =num}
    
    sort <- pickState[order(pickState[,column],pickState[,2]),]
    hospital <- sort[target,2]
    df <- rbind(df, data.frame(hospital,state=i))
  }
  return (df)
}