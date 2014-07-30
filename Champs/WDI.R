library(ggplot2)
library(WDI)
library(data.table)
library(RColorBrewer)
library (scales)
library(stringr)
library(reshape2)
library(xtable)

# NY.GDP.MKTP.KD.ZG -> "GDP growth (annual %)"  
DF <- WDI(country=c("XD","XM","XT","XN"), indicator="NY.GDP.MKTP.KD.ZG", start=1961, 
          end=2012)
# growth by income levels
qplot(year, NY.GDP.MKTP.KD.ZG, data = DF, geom = "line", color=country, xlab = "Year",
      ylab = "Growth Rates", title="Annual GDP Growth rate (%)")

# growth_rate  <- "NY.GDP.MKTP.KD.ZG"
# world <- "1W"
# high_income  <- "XD"
# middle_income <- "XP"
# low_income  <- "XM"
# upper_middle_income  <- "XT"
# lower_middle_income  <- "XN"

#  [87,] "GDP growth (annual %)" 
# "NY.GDP.MKTP.KD.ZG"
x  <- WDI(indicator="NY.GDP.MKTP.KD.ZG", start = 1961, end = 2012)
data <- data.table(x)
data  <- na.omit(data)
countrycompared <- "World"
data2 <- data[, 
              ToKeep := NY.GDP.MKTP.KD.ZG >= .SD[country == countrycompared, 
                                                 NY.GDP.MKTP.KD.ZG],
              by = year] [ToKeep == T]

Count  <- rle(data2$iso2c)
data2$Count <- Count[[1]][match(data2$iso2c, Count[[2]])]
# take countries that have grown faster than average world growth at least 75% of the time
# and exclude "countries" that are regions
regions <- c("1A", "S3", "Z4", "4E", "XC", "Z7", "7E", "EU", "XE", "XD", "XR", "XS", "ZJ",
             "XJ", "XL", "XO", "XM", "XN", "ZQ", "XQ", "XP", "XU", "OE", "S4", "S2", "S1",
             "8S", "ZG", "ZF", "XT")
data3 <- subset(data2, Count >=0.75*max(Count) & iso2c %in% regions == FALSE)
write.table(data3, file="data2.csv", sep=";")

ggplot(data3, aes(year, NY.GDP.MKTP.KD.ZG, color = country)) + geom_line() + 
  xlab('Year') + ylab('GDP growth rate') +
  labs(title = "Annual GDP Growth rate (%)") +
  theme_bw() +
  geom_line(data=subset(data3, country == "World"), colour="black", size=1) +
  guides(colour=guide_legend(override.aes=list(
    colour=c(hue_pal()(11)[1:10], "black"), size=c(rep(1, 10), 1.5))))

  
# [94,] "GDP per capita (constant 2000 US$)" NY.GDP.PCAP.KD
#  [97,] "GDP per capita, PPP (current international $)"  NY.GDP.PCAP.PP.CD

beg <-  WDI(indicator="NY.GDP.PCAP.PP.CD", start = 1980, end = 1980)
end <-  WDI(indicator="NY.GDP.PCAP.PP.CD", start = 2012, end = 2012)
merged <- merge(beg, end, all = TRUE)
merged <- subset(merged, iso2c %in% regions == FALSE)
reshaped <- na.omit(dcast(merged, country ~ year, value.var="NY.GDP.PCAP.PP.CD"))
names(reshaped) <- c("country", "beg", "end")
total <- transform(reshaped, total.growth = end / beg)
total <- total[order(total$total.growth, decreasing = T),]
names(total) <- c("Country","GDP (1980)", "GDP (2012)", "Growth Multiple")
top <- head(total, 10)
tbl <- xtable(top)

print.xtable(tbl, type = getOption("xtable.type", "html"),
             file = getOption("xtable.file", "test_html_table"),
             include.rownames = getOption("xtable.include.rownames", FALSE),
             html.table.attributes = getOption("xtable.html.table.attributes","border=0"))
           

