data <- read.csv("WHO.csv")
mean(data$Over60)
data[data$Over60 == min(data$Over60),]

maxValue <- max(data$LiteracyRate, na.rm=TRUE)
data[which(data$LiteracyRate== maxValue),]

crimeData <- read.csv("mvtWeek1.csv")
max(crimeData$ID)
min(crimeData$Beat)
table(crimeData$Arrest)
table(crimeData$LocationDescription=="ALLEY")
DateConvert = as.Date(strptime(crimeData$Date, "%m/%d/%y %H:%M"))

crimeData$Month = months(DateConvert)
crimeData$Weekday = weekdays(DateConvert)
crimeData$Date = DateConvert
table(crimeData$Month)
table(crimeData$Weekday)

library(plyr)
arrests <- ddply(crimeData, .(Month), summarize,  arrest=sum(Arrest))
max(arrests$arrest)
# or
table(crimeData$Arrest,crimeData$Month)

# crimeData$Year <-  as.numeric(format(crimeData$Date, "%Y")) 
# periods <- ddply(crimeData, .(Year), summarize,  arrest=sum(Arrest))
boxplot(periods$arrest~periods$Year)

table(crimeData$Arrest, crimeData$Year)

sort(table(crimeData$LocationDescription), decreasing = TRUE)

top5 <- crimeData[crimeData$LocationDescription %in% 
                      c("STREET", "PARKING LOT/GARAGE(NON.RESID.)",
                        "ALLEY","GAS STATION", "DRIVEWAY - RESIDENTIAL"),]

top5$LocationDescription = factor(top5$LocationDescription)
table(top5$LocationDescription, top5$Arrest)

table(top5$LocationDescription, top5$Weekday)
#########################################################

IBM <- read.csv("./data/IBMStock.csv")
GE <- read.csv("./data/GEStock.csv")
ProcterGamble <- read.csv("./data/ProcterGambleStock.csv")
CocaCola <- read.csv("./data/CocaColaStock.csv")
Boeing <- read.csv("./data/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date, CocaCola$StockPrice, type="line", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="dark blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1983-12-31")), lwd=2)


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="dark blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="orange")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="green")
legend("topright",
       c("Coca Cola", "PG", "IBM", "GE","Boeing"),
       lty=1, lwd= 2.5,col=c("red","dark blue", "orange", "purple", "green"))
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

IBM$Months <- months(IBM$Date)
tapply(IBM$StockPrice, IBM$Months, mean)
mean(IBM$StockPrice)
priceMonthIBM <- ddply(IBM, .(Months), summarize,  mean= mean(StockPrice),
                    above = ifelse(mean-mean(IBM$StockPrice)>0, "Above", ""))

GE$Months <- months(GE$Date)
priceMonthGE <- ddply(GE, .(Months), summarize,  mean= mean(StockPrice),
                      above = ifelse(mean-mean(GE$StockPrice)>0, "Above", ""))

################################################################################

poll <- read.csv("./data/AnonymityPoll.csv")
table(poll$Smartphone)
sum(is.na(poll$Smartphone))

table(poll$State, poll$Region=="South")

table(poll$Smartphone,poll$Internet.Use, useNA="ifany")
sum(is.na(poll$Smartphone))
sum(is.na(poll$Internet.Use))

limited = subset(poll, Internet.Use == 1 | Smartphone == 1)

count(limited$Info.On.Internet == 11)
table(limited$Privacy.Laws.Effective)

hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)

max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

table(limited$Smartphone, limited$Tried.Masking.Identity)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
###########################################################################

CPS <- read.csv("./data/CPSData.csv")
sort(table(CPS$State))
table(CPS$Citizenship)
table(CPS$Race, CPS$Hispanic)

table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
x <- tapply(is.na(CPS$MetroAreaCode), CPS$State,  mean)
which( x > 0.29 & x < 0.31)

MetroAreaCode <- read.csv("./data/MetroAreaCodes.csv")
CountryMap <- read.csv("./data/CountryCodes.csv")
CPS = merge(CPS, MetroAreaCode, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sum(is.na(CPS$MetroArea))
table(CPS$MetroArea)[c("Atlanta-Sandy Springs-Marietta, GA" , 
                            "Baltimore-Towson, MD" ,
                            "Boston-Cambridge-Quincy, MA-NH" ,
                            "San Francisco-Oakland-Fremont, CA")]

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing=TRUE)

x <- sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean), decreasing=TRUE)
length(x[x>0.2])

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
sum(is.na(CPS$Country))
sort(table(CPS$Country), decreasing=TRUE)

tapply(CPS$Country != "United States", 
    CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" , mean, na.rm=TRUE)

head(sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE), decreasing=T),3)

t.test(9, alternative="two.sided", mu=1100)
