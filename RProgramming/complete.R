complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  data <- data.frame()
  for (i in id){
    if (i < 10){full_id <- paste("00",i, sep="")}
    else if (i<100){full_id <- paste("0",i, sep="")}
    else {full_id <- i}
    url <- paste("./",  directory, "/", full_id, ".csv",sep="")
    readUrl <- read.csv(url)
    freq <- count(complete.cases(readUrl))
    res <- data.frame(i, freq[2,2])
    data <- rbind(data, res)
    
  }
  colnames(data) <- c("id","nobs")
  return (data)
}