library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(RTextTools)

wiki <- read.csv("./data/wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
# prepend all the words with a letter a
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved , 0.997)
wordsRemoved  <- as.data.frame(as.matrix(sparseRemoved ))
# prepend all the words with a letter r
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
splitWiki <- sample.split(wikiWords$Vandal, SplitRatio=0.7)
trainWiki <- subset(wikiWords, splitWiki==TRUE)
testWiki <- subset(wikiWords, splitWiki==FALSE)
table(testWiki$Vandal)

wikiTree <- rpart(Vandal ~., data=trainWiki, method="class")
wikiPredict <- predict(wikiTree, newdata=testWiki, type="class")
table(testWiki$Vandal, wikiPredict)
prp(wikiTree)

x <- predict(wikiTree, type="class")
table(trainWiki$Vandal, x)
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
wikiTrain2 = subset(wikiWords2, splitWiki==TRUE)
wikiTest2 = subset(wikiWords2, splitWiki==FALSE)
wikiTree2 <- rpart(Vandal ~., data=wikiTrain2, method="class")
wikiPredict2 <- predict(wikiTree2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, wikiPredict2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)
wikiTrain3 = subset(wikiWords2, splitWiki==TRUE)
wikiTest3 = subset(wikiWords2, splitWiki==FALSE)
wikiTree3 <- rpart(Vandal ~., data=wikiTrain3, method="class")
wikiPredict3 <- predict(wikiTree3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, wikiPredict3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, splitWiki==TRUE)
wikiTest4 = subset(wikiWords3, splitWiki==FALSE)
wikiTree4 <- rpart(Vandal ~., data=wikiTrain4, method="class")
wikiPredict4 <- predict(wikiTree4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, wikiPredict4)
prp(wikiTree4)

####################################
# Clinical Trials (Pubmed Data)
####################################

trials <- read.csv("./data/clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(trials$abstract))
table(nchar(trials$abstract)==0)
subset(trials,nchar(trials$title) == min(nchar(trials$title)))[,1]
# or
trials[which.min(nchar(trials$title)),][,1]

corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle  <- tm_map(corpusTitle , tolower)
corpusAbstract  <- tm_map(corpusAbstract, tolower)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

frequencyAbstract <- colSums(dtmAbstract)
which.max(frequencyAbstract)
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

set.seed(144)
trialsSplit <- sample.split(dtm$trial, SplitRatio=0.7)
trailsTrain <- subset(dtm, trialsSplit==T)
trailsTest<- subset(dtm, trialsSplit==F)
table(trailsTrain$trial)

trialsTree <- rpart(trial~., data=trailsTrain, method="class")
prp(trialsTree)
trainPred <- predict(trialsTree)
max(trainPred[,2])
table(trailsTrain$trial, trainPred[,2]>0.5)
testPred <- predict(trialsTree, newdata=trailsTest)
table(trailsTest$trial, testPred[,2]>0.5)

predROCRtrials = prediction(testPred[,2], trailsTest$trial)
performance(predROCRtrials, "auc")@y.values


####################################
# Spam
####################################

emails <- read.csv("./data/emails.csv", stringsAsFactors=F)
table(emails$spam)
max(nchar(emails$text))
which.min(nchar(emails$text))

corpus = Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam <- emails$spam
hamEmails <- subset(emailsSparse, emailsSparse$spam==0)
table(colSums(hamEmails) > 5000)

spamEmails <- subset(emailsSparse[,1:330], emailsSparse$spam==1)
table(colSums(spamEmails) > 1000)
# or
sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
emailsSplit <- sample.split(emailsSparse$spam, SplitRatio=0.7)
emailsTrain <- subset(emailsSparse, emailsSplit==TRUE)
emailsTest <- subset(emailsSparse, emailsSplit==FALSE)

spamLog <- glm(spam~.,data=emailsTrain, family="binomial")
spamCART <- rpart(spam~.,data=emailsTrain, method="class")
spamRF <- randomForest(spam~.,data=emailsTrain)

spamLogPred <- predict(spamLog, type="response")
spamCARTPred <- predict(spamCART)
spamRFPred <- predict(spamRF, type="prob")

table(spamLogPred <0.00001)
table(spamLogPred > 0.99999 & spamLogPred <0.00001)
prp(spamCART)

table(emailsTrain$spam, spamLogPred > 0.5)
predROCR = prediction(spamLogPred, emailsTrain$spam)
performance(predROCR, "auc")@y.values

table(emailsTrain$spam, spamCARTPred[,2] > 0.5)
predROCR2 = prediction(spamCARTPred[,2], emailsTrain$spam)
performance(predROCR2, "auc")@y.values

table(emailsTrain$spam, spamRFPred[,2] > 0.5)
predROCR3 = prediction(spamRFPred[,2], emailsTrain$spam)
performance(predROCR3, "auc")@y.values

# on testing set

spamLogPred2 <- predict(spamLog, type="response", newdata=emailsTest)
spamCARTPred2 <- predict(spamCART, newdata=emailsTest)
spamRFPred2 <- predict(spamRF, type="prob", newdata=emailsTest)

table(emailsTest$spam, spamLogPred2 > 0.5)
predROCR4 = prediction(spamLogPred2, emailsTest$spam)
performance(predROCR4, "auc")@y.values

table(emailsTest$spam, spamCARTPred2[,2] > 0.5)
predROCR5 = prediction(spamCARTPred2[,2], emailsTest$spam)
performance(predROCR5, "auc")@y.values

table(emailsTest$spam, spamRFPred2[,2] > 0.5)
predROCR6 = prediction(spamRFPred2[,2], emailsTest$spam)
performance(predROCR6, "auc")@y.values

wordCount = rowSums(as.matrix(dtm))
hist(wordCount, breaks=50)
hist(log(wordCount), breaks=50)

wordCountSparse <- rowSums(as.matrix(spdtm))
logWordCount <- log(wordCountSparse)
boxplot(logWordCount ~ emailsSparse$spam)
emailsSparse$logWordCount <- logWordCount

emailsTrain2 <- subset(emailsSparse, emailsSplit==TRUE)
emailsTest2 <- subset(emailsSparse, emailsSplit==FALSE)

spam2CART <- rpart(spam~.,data=emailsTrain2, method="class")
set.seed(123)
spam2RF <- randomForest(spam~.,data=emailsTrain2)

prp(spam2CART)
spam2CARTPred <- predict(spam2CART, newdata=emailsTest2)
spam2RFPred <- predict(spam2RF, newdata=emailsTest2, type="prob")

table(emailsTest2$spam, spam2CARTPred[,2] > 0.5)
predROCR7 = prediction(spam2CARTPred[,2], emailsTest2$spam)
performance(predROCR7, "auc")@y.values

table(emailsTest2$spam, spam2RFPred[,2] > 0.5)
predROCR8 = prediction(spam2RFPred[,2], emailsTest2$spam)
performance(predROCR8, "auc")@y.values

# create a document term matrix containing all 2-grams in dataset
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
# remove terms that appear infrequently
spdtm2gram <- removeSparseTerms(dtm2gram, 0.95)
# build data frame
emailsSparse2gram <- as.data.frame(as.matrix(spdtm2gram))
# Convert the column names of emailsSparse2gram to valid names
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))
# Combine the original emailsSparse with emailsSparse2gram into a final data frame
emailsCombined = cbind(emailsSparse, emailsSparse2gram)
# Use the same sample.split output obtained earlier to split emailsCombined into 
# a training and testing set
trainCombined <- subset(emailsCombined, emailsSplit==TRUE)
testCombined <- subset(emailsCombined, emailsSplit==FALSE)
# Use trainCombined to train a CART tree with the default parameters
spamCARTcombined <- rpart(spam~.,data=trainCombined, method="class")
# Use trainCombined to train a random forest with the default parameters
set.seed(123)
spamRFcombined <- randomForest(spam~.,data=trainCombined)
# create tree with full variable names
prp(spamCARTcombined, varlen=0)
# What is the test-set accuracy of spamCARTcombined, using a threshold of 0.5 
# for predictions
spamCARTcombinedPred <- predict(spamCARTcombined, newdata=testCombined)
table(testCombined$spam, spamCARTcombinedPred[,2]>0.5)
predROCR9 = prediction(spamCARTcombinedPred[,2], testCombined$spam)
performance(predROCR9, "auc")@y.values

# What is the test-set accuracy of spamRFcombined, using a threshold of 0.5
spamRFcombinedPred <- predict(spamRFcombined, newdata=testCombined, type="prob")
table(testCombined$spam, spamRFcombinedPred[,2]>0.5)
predROCR10 = prediction(spamRFcombinedPred[,2], testCombined$spam)
performance(predROCR10, "auc")@y.values
