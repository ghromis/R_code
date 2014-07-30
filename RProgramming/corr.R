corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
    res <- vector()
    for (i in 1:332){
      if (i < 10){full_id <- paste("00",i, sep="")}
      else if (i<100){full_id <- paste("0",i, sep="")}
      else {full_id <- i}
      url <- paste("./",  directory, "/", full_id, ".csv",sep="")
      data <- read.csv(url)
      completeData <- data[complete.cases(data),]
      if (nrow(completeData) > threshold){
        corr <- cor(completeData$sulfate, completeData$nitrate) 
        res <- append(res, corr)}
    }   
    return (res)
}