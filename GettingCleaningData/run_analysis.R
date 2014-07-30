# get urls, read into r and bind into one dataset.
trainSetUrl <- "./UCI HAR Dataset/train/X_train.txt"
trainLabelUrl <- "./UCI HAR Dataset/train/y_train.txt"
trainSubjectUrl <- "./UCI HAR Dataset/train/subject_train.txt"

testSetUrl <- "./UCI HAR Dataset/test/X_test.txt"
testLabelUrl <- "./UCI HAR Dataset/test/y_test.txt"
testSubjectUrl <- "./UCI HAR Dataset/test/subject_test.txt"

featuresUrl <- "./UCI HAR Dataset/features.txt"
features <- read.table(featuresUrl, header=F)

trainSet <- read.table(trainSetUrl, header=F)
colnames(trainSet) <- features$V2
trainLabel <- read.table(trainLabelUrl, header=F)
trainSubject <- read.table(trainSubjectUrl, header=F)
train <- cbind(trainSet,trainLabel, trainSubject)

testSet <- read.table(testSetUrl, header=F)
colnames(testSet) <- features$V2
testLabel <- read.table(testLabelUrl, header=F)
testSubject <- read.table(testSubjectUrl, header=F)
test <- cbind(testSet, testLabel, testSubject)

mergedData <-rbind(train,test)
# change column names to it's descriptive names
colnames(mergedData)[562] <- "Label"
colnames(mergedData)[563] <- "Subject"
allLabels <- c("walking", "walking upstairs", "walking downstairs",
              "sitting", "standing", "laying")
for (i in 1:6){
  mergedData$Label[mergedData$Label== i] <- allLabels[i]
}

# extract required features
extractedColumns <- grep("mean\\(\\)|std\\(\\)", features$V2)
extractedValues <- mergedData[c(extractedColumns, 562,563)]

# create tidy data set and read into separate file
tidySet <- aggregate(. ~ Label+Subject, data = extractedValues, mean)

write.csv(means, file="tidySet.csv", row.names=FALSE)
write.table(means, file="tidySet.txt", row.names=FALSE)





