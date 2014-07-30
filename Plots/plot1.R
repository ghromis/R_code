url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

png("plot1.png", width = 480, height = 480)
hist(as.numeric(subset$Global_active_power), col="red", 
     main = "Global Active Power",xlab="Global Active Power (kilowatts)")
dev.off()

