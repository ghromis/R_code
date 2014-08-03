quality = read.csv("./data/quality.csv")
#install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, 
                 data=qualityTrain, family=binomial)


############################
# Songs #
#############################

songs <- read.csv("./data/songs.csv")
dim(subset(songs, songs$year==2010))
dim(subset(songs, songs$artistname=="Michael Jackson"))

subset(songs, songs$artistname=="Michael Jackson" & 
                songs$Top10==1)$songtitle
table(songs$timesignature)
subset(songs, songs$tempo==max(songs$tempo))

SongsTest <- subset(songs, songs$year==2010)
SongsTrain <- subset(songs, !songs$year %in% SongsTest$year)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
TestPrediction = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, TestPrediction >= 0.45)
# accuracy
(309+19)/(349+24)

table(SongsTest$Top10)

########################################################################
############################
# Baseball #
#############################

baseball <- read.csv("./data/baseball.csv")
length(unique(baseball$Year))
baseball <- subset(baseball, baseball$Playoffs ==1)

PlayoffTable <- table(baseball$Year)
# or
table(table(baseball$Year))

names(PlayoffTable)
PlayoffTable[c("1990","2001")]

baseball$NumCompetitors <- PlayoffTable[as.character(baseball$Year)]

dim(subset(baseball, baseball$NumCompetitors==8))

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
dim(subset(baseball, baseball$WorldSeries==0))

b1 = glm(WorldSeries ~ League, data=baseball, family=binomial)
summary(b1)

b2 <- b2 <- glm(WorldSeries ~ Year+RA+RankSeason+NumCompetitors, 
                data=baseball, family=binomial)
summary(b2)

cor(baseball[,3:17])
# or
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

b3 <- glm(WorldSeries ~ RankSeason+NumCompetitors, data=baseball, family=binomial)
summary(b3)

min(c(232.35, 237.88, 238.75, 230.96, 233.88, 233.55, 238.22, 232.74,232.52))

########################################################################
############################
# Parole #
#############################

parole <- read.csv("./data/parole.csv")
dim(subset(parole, parole$violator==1))
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

par1 <- glm(violator~., data=train, family="binomial")

# if multiple.offences coefficient is 1.61, it means that parolee who commited
# multiple offences has e^1.61 times higher odds of being a violator than 
# a parolee who did not commit multiple offenses but is otherwise identical.

paroleData <- read.csv("./data/parole.test.csv")
paroleData$state <- as.factor(paroleData$state)
paroleData$crime <- as.factor(paroleData$crime)
parolePred <- predict(par1, newdata=paroleData, type="response")
# or
odds_parole <- exp(-4.2411574 +0.3869904+0.8867192 -0.0001756*50-0.1238867*3+
               0.0802954*12+0.6837143*1)
prob_parole <- odds_parole/(1+odds_parole)

max(predict(par1, newdata=test, type="response"))

parolePred2 <- predict(par1, newdata=test, type="response")
table(test$violator, parolePred2 > 0.2)

# parole model sensitivity
12/23
# specificity
167/179
#accuracy
179/(179+23)

library(ROCR)
ROCRpred = prediction(parolePred2, test$violator)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

########################################################################
############################
# Loans #
#############################

loans <- read.csv("./data/loans.csv")
nfp <- subset(loans, loans$not.fully.paid==1)

missing = dim(subset(loans, complete.cases(loans)==FALSE))

library(mice)
set.seed(144)
# set vars.for.imputation to all variables in the data frame except for 
# not.fully.paid
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loansTrain = subset(loans, split == TRUE)
loansTest = subset(loans, split == FALSE)

loansPred <- glm(not.fully.paid~.,data=loansTrain, family="binomial")
summary(loansPred)

A=-9.288e-03*700
B=-9.288e-03*710

predicted.risk <- predict(loansPred, newdata=loansTest, type="response")
loansTest$predicted.risk <- predicted.risk
table(loansTest$not.fully.paid, predicted.risk>0.5)
# accuracy
2403/(2416+457)

library(ROCR)
ROCRpredLoan = prediction(predicted.risk, loansTest$not.fully.paid)
auc = as.numeric(performance(ROCRpredLoan, "auc")@y.values)

loansPred2 <- glm(not.fully.paid~int.rate,data=loansTrain, family="binomial")
predicted.risk2 <- predict(loansPred2, newdata=loansTest, type="response")
table(predicted.risk2>0.5)

loansTest$profit = exp(loansTest$int.rate*3) - 1
loansTest$profit[loansTest$not.fully.paid == 1] = -1

highInterest <- subset(loansTest, loansTest$int.rate >= 0.15)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <=cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
