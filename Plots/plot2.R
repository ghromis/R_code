url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")

png("plot2.png", width = 480, height = 480)
plot(dateTime, as.numeric(subset$Global_active_power), type="l",
     ylab="Global Active Power (kilowatts)", xlab="")
dev.off()