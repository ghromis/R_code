url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")

png("plot4c.png", width = 480, height = 480)
par(mfrow = c(2, 2)) 
plot(dateTime, as.numeric(subset$Global_active_power), type="l",
     ylab="Global Active Power", xlab="", cex=0.2)
plot(dateTime, as.numeric(subset$Voltage), type="l",
     ylab="Voltage", xlab="datetime")
plot(dateTime, as.numeric(subset$Sub_metering_1), type="l",
     ylab="Energy Submetering", xlab="")
lines(dateTime, as.numeric(subset$Sub_metering_2), type="l", col="red")
lines(dateTime, as.numeric(subset$Sub_metering_3), type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd =2.5,
       col=c("black", "red", "blue"), bty = "n")
plot(dateTime, as.numeric(subset$Global_reactive_power), type="l", xlab="datetime",
     ylab="Global_reactive_power")
dev.off()

