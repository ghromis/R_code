# Simple histogram
outcome <- read.csv("./dataSource/outcome-of-care-measures.csv", 
                    colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main ="Heart Attack 30−day Death Rate", 
     xlab="30−day Death Rate")

# 3-panel histogram with median line and density line
heartAttackMortality <- na.omit(as.numeric(outcome[, 11]))
heartFaliureMortality <- na.omit(as.numeric(outcome[, 17]))
pneumoniaMortality <- na.omit(as.numeric(outcome[, 23]))
meanHA <- mean(heartAttackMortality)
meanHF <- mean(heartFaliureMortality)
meanP <- mean(pneumoniaMortality)

par(mfrow = c(3, 1)) 
hist(heartAttackMortality, 
     main =substitute(paste("Heart Attack (", hat(X), "=",x, ")"), env=list(x=meanHA)),  
     xlab="30−day Death Rate", xlim=c(5,20), prob=TRUE)
abline(v=median(heartAttackMortality),col="red", lwd=2.5, lty=2)
lines(density(heartAttackMortality), col="blue", lwd=2) 

hist(heartFaliureMortality, 
     main =substitute(paste("Heart Faliure (", hat(X), "=",x, ")"), env=list(x=meanHF)), 
     xlab="30−day Death Rate", xlim=c(5,20), prob=TRUE)
abline(v=median(heartFaliureMortality),col="red", lwd=2.5, lty=2)
lines(density(heartFaliureMortality), col="blue", lwd=2) 

hist(heartFaliureMortality, 
     main =substitute(paste("Pneumonia (", hat(X), "=",x, ")"), env=list(x=meanP)), 
     xlab="30−day Death Rate", xlim=c(5,20), prob=TRUE)
abline(v=median(pneumoniaMortality),col="red", lwd=2.5, lty=2)
lines(density(heartFaliureMortality), col="blue", lwd=2) 

# boxplot sorted by median
table(outcome$State)
temp <- with(outcome, table(State))
statesWith20 <- temp[temp>20]
nameVector <- as.vector(names(statesWith20))
outcome2 <- subset(outcome, outcome$State %in% nameVector )
df <- outcome2[,c("State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
colnames(df) <- c("state","death")
df <- na.omit(data.table(df))
bymedian <- with(na.omit(df), reorder(as.factor(state), death, median))
boxplot(na.omit(death) ~ bymedian, data = df,
        main="Heart Attack 30-day Death Rate by State",
        ylab="30-day Death Rate", las=2, cex.axis=0.6)

# Plots with lattice
library(lattice)
outcome <- read.csv("./dataSource/outcome-of-care-measures.csv", 
                    colClasses = "character")
hospital <- read.csv("./dataSource/hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11])
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
xyplot(death ~ npatient | owner, outcome.hospital, 
       type = c("p","r"),
      main="Heart Attack 30−day Death Rate by Ownership",
      xlab="Number of Patients Seen",
      ylab="30−day Death Rate")

xyplot(death ~ npatient | owner,
       panel = function(...) {
         panel.xyplot(...)
         panel.lmline(..., col="red")
       },
       main="Heart Attack 30−day Death Rate by Ownership",
       xlab="Number of Patients Seen",
       ylab="30−day Death Rate"
       )

