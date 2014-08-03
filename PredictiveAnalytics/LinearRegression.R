data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  
                  state.division, state.name, state.region)

plot(statedata$x, statedata$y, col = "dark blue")
tapply(statedata$HS.Grad, statedata$state.region, mean)
boxplot(statedata$Murder ~ statedata$state.region)
outlier <- subset(statedata, state.region == "Northeast")
outlier[outlier$Murder == max(outlier$Murder),]

lifeExp <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
                  Frost + Area, data = statedata)
plot(statedata$Income, statedata$Life.Exp)

# backwards variable selection
lifeExp2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
                  Frost, data = statedata)
lifeExp3 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + 
                   Frost, data = statedata)
lifeExp4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + 
                   Frost, data = statedata)
sort(predict(lifeExp4))
statedata[which.max(statedata$Life.Exp),]

sort(abs(lifeExp4$residual))

###############################################################################

climate <- read.csv("./data/climate_change.csv")
climateTraining <- subset(climate, climate$Year < 2007)
climateTest <- subset(climate, climate$Year > 2006)
tempExp <- lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=climateTraining)
summary(tempExp)
cor(climateTraining)

tempExp2 <- lm(Temp ~ MEI+N2O+TSI+Aerosols, data=climateTraining)

tempExp3 <- step(tempExp)
summary(tempExp3)

predictTemp <- predict(tempExp3, newdata=climateTest)
SSE = sum((predictTemp - climateTest$Temp)^2)
SST = sum((mean(climateTraining$Temp) - climateTest$Temp)^2)
R2 = 1 - SSE/SST
R2
###############################################################################

pisaTrain <- read.csv("./data/pisa2009train.csv")
pisaTest <- read.csv("./data/pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

pisalm <- lm(readingScore ~ ., data = pisaTrain)

pisaTrainSSE <- sum(pisalm$residuals^2)
pisaTrainRMSE <- sqrt(pisaTrainSSE/nrow(pisaTrain))
pisaTrainSST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

predictedScore <- predict(pisalm)

pisaPredictions <- predict(pisalm, newdata=pisaTest)
pisaSSE <- sum((pisaPredictions - pisaTest$readingScore)^2)
pisaRMSE <- sqrt(pisaSSE/nrow(pisaTest))
R <- 1 - pisaSSE / pisaTrainSST
################################################################################

FluTrain <- read.csv("./data/FluTrain.csv")
subset(FluTrain, FluTrain$Queries==max(FluTrain$Queries))
hist(FluTrain$ILI, col="light blue", breaks=20)
plot( FluTrain$Queries, log(FluTrain$ILI), col="dark green")

FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)

FluTest <- read.csv("./data/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FlueTest <- cbind(FluTest, PredTest1)
subset(FlueTest, FlueTest$Week == "2012-03-11 - 2012-03-17")
relativeError = (2.293422 - 2.187378) / 2.293422

FluSSE <- sum((PredTest1 - FluTest$ILI)^2)
FluRMSE <- sqrt(FluSSE/nrow(FluTest))

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
sum(is.na(FluTrain$ILILag2))

plot(log(FluTrain$ILILag2), log(FluTrain$ILI), col="salmon")
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)

ILILagTest2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILagTest2)
sum(is.na(FluTest$ILILag2))

FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
FluSSE2 <- sum((PredTest2 - FluTest$ILI)^2)
FluRMSE2 <- sqrt(FluSSE2/nrow(FluTest))
