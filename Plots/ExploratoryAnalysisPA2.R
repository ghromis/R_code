library(plyr)
library(ggplot2)

unzipf <- unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# 1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
totalE <- ddply(NEI, .(year), summarise, totalEmissions = sum(Emissions))
png("plot1.png", width = 480, height = 480)
plot(totalE$year,totalE$totalEmissions, type="b", col="#31a354",
     xlab="Year", ylab = "Total Pollution", main=expression(paste("Total Emissions from ", PM[2.5])))


# 2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
baltimoreData <- NEI[NEI$fips == "24510",]
baltimoreE <- ddply(baltimoreData, .(year), summarise, totalEmissions = sum(Emissions))
plot(baltimoreE$year,baltimoreE$totalEmissions, type="b", col="#31a354",
     xlab="Year", ylab = "Total Pollution", 
     main=expression(paste("Total Emissions from ", PM[2.5], " in Baltimore")))


# 3) Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?

emmisionByType <- ddply(baltimoreData, .(year, type), summarise, totalEmissions = sum(Emissions))
g <- qplot(year, totalEmissions, data = emmisionByType, color = type, geom = c("point", "line"))
g+ labs(y = "Total Emissions")

emmisionByType[emmisionByType$type=="POINT",]


# 4) Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?
values=c(grep("Coal", unique(SCC$EI.Sector), value=TRUE))
codes <- SCC[SCC$EI.Sector %in% values,]
targetSCC <- codes$SCC
coalData <- NEI[NEI$SCC %in% targetSCC,]
coalEmission <- ddply(coalData, .(year), summarise, totalEmissions = sum(Emissions))
plot(coalEmission$year,coalEmission$totalEmissions, type="b", col="#31a354",
     xlab="Year", ylab = "Total Pollution (tons)", 
     main=expression(paste("Total Emissions ", PM[2.5], " from Coal")))

# 5) How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?
values2=c(grep("On-Road", unique(SCC$EI.Sector), value=TRUE))
codes2 <- SCC[SCC$EI.Sector %in% values2,]
targetSCC2 <- codes2$SCC
vehicleDataBaltimore <- baltimoreData[baltimoreData$SCC %in% targetSCC2,]
vehicleEmission <- ddply(vehicleDataBaltimore, .(year), summarise, totalEmissions = sum(Emissions))
plot(vehicleEmission$year,vehicleEmission$totalEmissions, type="b", col="#31a354",
     xlab="Year", ylab = "Total Pollution (tons)", cex.main=1,
     main=expression(paste("Total Emissions ", PM[2.5], " from Motor Vehicles in Baltimore")))

# 6) Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == 06037). 
# Which city has seen greater changes over time in motor vehicle emissions?
laData <- NEI[NEI$fips == "06037",]
vehicleDataLA <- laData[laData$SCC %in% targetSCC2,]
vehicleDataBaLA <- rbind(vehicleDataBaltimore, vehicleDataLA)
vehicleEmissionBaLA <- ddply(vehicleDataBaLA, .(year, fips), summarise, totalEmissions = sum(Emissions))
vehicleEmissionBaLA$fips <- factor(vehicleEmissionBaLA$fips, levels=c("06037", "24510"), 
                              labels=c("LA","Baltimore"))

g <- qplot(year, totalEmissions, data = vehicleEmissionBaLA, color = fips, geom = c("point", "line"))
g+ facet_wrap(~fips, nrow = 1, ncol = 2) +
    labs(y = "Total Emissions")+ guides(color=guide_legend(title="City"))
   

