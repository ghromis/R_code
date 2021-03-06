pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values) 
  data <- data.frame()
  for (i in id){
    if (i < 10){full_id <- paste("00",i, sep="")}
    else if (i<100){full_id <- paste("0",i, sep="")}
    else {full_id <- i}
    url <- paste("./",  directory, "/", full_id, ".csv",sep="")
    readUrl <- read.csv(url)
    data <- rbind(data, readUrl)
  }
    if (pollutant == "sulfate"){
      return (mean(na.omit(data$sulfate)))}
    else {
      return (mean(na.omit(data$nitrate)))}  
}
