library(rpart)
library(caTools)
library(e1071)
library(caret)
library(randomForest)

gerber <- read.csv("./data/gerber.csv")
nrow(subset(gerber, gerber$voting==1)) / nrow(gerber)
nrow(subset(gerber, gerber$voting==1 & gerber$hawthorne==1))
nrow(subset(gerber, gerber$voting==1 & gerber$civicduty==1))
nrow(subset(gerber, gerber$voting==1 & gerber$self==1))
nrow(subset(gerber, gerber$voting==1 & gerber$neighbors==1))

logReg <- glm(voting ~ hawthorne + civicduty + self + neighbors, 
              data=gerber, family=binomial)
logPred <- predict(logReg, type="response")
table(gerber$voting, logPred > 0.5)

ROCRpred = prediction(logPred, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# There are no splits in the tree, because none of the variables make a big 
# enough effect to be split on.
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# cp param forces tree to be built
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, 
                   data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, 
                   data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

# men
0.345818-0.302795
# women
0.334176-0.290456

logReg2 <- glm(voting ~ control + sex, data=gerber, family=binomial)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logReg2, newdata=Possibilities, type="response")


# new term to our logistic regression now, that is the combination of the "sex" 
# and "control" variables - so if this new variable is 1, that means the person 
# is a woman AND in the control group.
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
predict(LogModel2, newdata=Possibilities, type="response")


###########################################
# multiclass classification problems
# Letter REcognition #
#############################################

letters <- read.csv("./data/letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain <- subset(letters, split==T)
lettersTest <- subset(letters, split==F)

CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
CARTbPredict <- predict(CARTb, newdata=lettersTest, type="class")
table(lettersTest$isB, CARTbPredict)

set.seed(1000)
lettersRF <- randomForest(isB ~ . - letter, data=lettersTrain)
lettersRFPredict <- predict(lettersRF, newdata=lettersTest, type="class")
table(lettersTest$isB, lettersRFPredict)

# Multivariable classification
letters <- letters[-18]
letters$letter = as.factor( letters$letter )
set.seed(2000)
splitM = sample.split(letters$letter, SplitRatio = 0.5)
lettersTrainM <- subset(letters, splitM==T)
lettersTestM <- subset(letters, splitM==F)
table(lettersTestM$letter)

CARTm = rpart(letter ~ ., data=lettersTrainM, method="class")
CARTmPredict <- predict(CARTm, newdata=lettersTestM, type="class")
table(lettersTest$letter, CARTmPredict)

###########################################
# Census Data #
#############################################

library(caTools)
library(rpart)
library(ROCR)
library(rpart.plot)
library(caret)
library(e1071)

census <- read.csv("./data/census.csv")
set.seed(2000)
splitC <- sample.split(census$over50k, SplitRatio=0.6)
censusTrain <- subset(census, splitC == T)
censusTest <- subset(census, splitC == F)

censusLogReg <- glm(over50k~., data=censusTrain, family="binomial")
censusLogRegPred <- predict(censusLogReg, newdata=censusTest, type="response")
table(censusTest$over50k, censusLogRegPred>0.5)  
table(censusTest$over50k)
ROCRpredCensus = prediction(censusLogRegPred, censusTest$over50k)
ROCRperfCensus = performance(ROCRpredCensus, "tpr", "fpr")
plot(ROCRperfCensus, colorize=TRUE)
auc = as.numeric(performance(ROCRpredCensus, "auc")@y.values)

censusTree <- rpart(over50k~., data=censusTrain, method="class")
prp(censusTree)
censusTreePred <- predict(censusTree, newdata=censusTest)
table(censusTest$over50k, censusTreePred[,2]>0.5)     
ROCRpredTree = prediction(censusTreePred[,2], censusTest$over50k)
ROCRperfTree = performance(ROCRpredTree, "tpr", "fpr")
plot(ROCRperfTree, colorize=TRUE)
auc = as.numeric(performance(ROCRpredTree, "auc")@y.values)

set.seed(1)
trainSmall = censusTrain[sample(nrow(censusTrain), 2000), ]
censusRF <- randomForest(over50k~.-nativecountry, data=trainSmall)
censusRFPred <- predict(censusRF, newdata=censusTest)
table(censusTest$over50k, censusRFPred)

#  number of times, aggregated over all of the trees in the random forest model, 
# that a certain variable is selected for a split
vu = varUsed(censusRF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusRF$forest$xlevels[vusorted$ix]))

varImpPlot(censusRF)

set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr.control = trainControl(method = "cv", number = 10)
tr <- train(over50k~., data = censusTrain, method = "rpart", 
            trControl = tr.control, tuneGrid = cartGrid)

censusTree2 <- rpart(over50k~., data=censusTrain, method="class", cp=0.002)
censusTreePred2 <- predict(censusTree2, newdata=censusTest)
table(censusTest$over50k, censusTreePred2[,2]>0.5) 
prp(censusTree2)
