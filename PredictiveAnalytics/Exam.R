library(caTools)
library(caret)
library(rpart.plot)
library(ROCR)
library(tm)
library(ggplot2)

elantra <- read.csv("elantra.csv")
train <- subset(elantra, elantra$Year < 2013)
test <- subset(elantra, elantra$Year == 2013)

fit1 <- lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries+Month, data=train)
summary(fit1)

train$Month <- as.factor(train$Month)
fit2 <- lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries+Month, data=train)
summary(fit2)

train$Month <- as.numeric(train$Month)
cor(train)

fit3 <- lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Month, data=train)
summary(fit3)

test$Month <- as.factor(test$Month)
fit3_predict <- predict(fit3, newdata=test)

# What is the test set R-Squared?
fit3SSE <- sum((fit3_predict - test$ElantraSales)^2)
fit3SST = sum((mean(train$ElantraSales) - test$ElantraSales)^2)
R <- 1 - fit3SSE / fit3SST

# What is the largest absolute error that we make in our test set predictions?
max(abs(fit3_predict - test$ElantraSales))

#####################################################

articles <- read.csv("nytimes.csv", stringsAsFactors=FALSE)

n <- nchar(articles$snippet)
nhigh <- subset(n, n>=100)
length(nhigh) / length(n)

headlineNchar <- nchar(articles$headline)
articles <- cbind(articles, headlineNchar)

cor(articles$headlineNchar, articles$popular)

articles$type <- as.factor(articles$type)
articles$popular <- as.factor(articles$popular)

set.seed(144)
spl <- sample.split(articles$popular, SplitRatio=0.7)
train <- subset(articles, spl==TRUE)
test <- subset(articles, spl==FALSE)

glm1 <- glm(popular~print+type+word.count, data=train, family="binomial")
summary(glm1)

odds = exp(-2.50756 -0.84683 + 0.90559+ 0.00026*682)
prob <- odds/(1+odds)

articlesPredict <- predict(glm1, newdata=test, type="response" )
table(articlesPredict >0.5)
table(test$popular)


ROCRpred = prediction(articlesPredict, test$popular)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

set.seed(144)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (1:50)*0.01)
tr = train(popular~print+type+word.count, data = train, method = "rpart", 
           trControl = tr.control, tuneGrid = cp.grid)

tree <- rpart(popular~print+type+word.count, data = train, cp=0.01)
prp(tree)

corpus <- Corpus(VectorSource(articles$snippet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)

articleText <- as.data.frame(as.matrix(spdtm))
f <- colSums(articleText)

articleText <- cbind(articleText, articles$print, articles$type,
                     articles$word.count, articles$popular)
colnames(articleText)[21] <- "popular"

spl <- sample.split(articleText$popular, SplitRatio=0.7)
trainText <- subset(articleText, spl==TRUE)
testText <- subset(articleText, spl==FALSE)

glmText <- glm(popular~., data=trainText, family="binomial")

textPredict <- predict(glmText, newdata= testText, type="response")

ROCRpred = prediction(textPredict, testText$popular)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

#####################################################

stocks <- read.csv("nasdaq_returns.csv")

sort(table(stocks$industry))

dec <- stocks$ret2000.12
length(subset(dec, dec>=0.1))
length(subset(dec, dec<=-0.1))

sort(tapply(stocks$ret2008.10, stocks$industry, mean))
sort(tapply(stocks$ret2000.02, stocks$industry, mean))

limited <- stocks[,4:123]
totalReturn <- sort(colMeans(limited))

limitedM <- as.matrix(limited)
limitedV <- as.vector(limited)

distance <- dist(limitedV, method = "euclidean")
hcluster <- hclust(distance, method="ward.D")
plot(clusterIntensity)

topclusters <- rect.hclust(hcluster, k=5, border="red")
cutFive <- cutree(hcluster, k=5)
table(cutFive)

stocks <- cbind(stocks, cutFive)
addmargins(table(stocks$industry, stocks$cutFive))

addmargins(table(stocks$subindustry, stocks$cutFive))

tapply(rowMeans(limited), stocks$cutFive, mean)

tapply(stocks$ret2000.02, stocks$cutFive, mean)
tapply(stocks$ret2009.12, stocks$cutFive, mean)

boxplot(stocks$ret2000.02~stocks$cutFive)
ggplot(stocks, aes(x=cutFive, y=ret2000.02))+geom_point(size=5, col="blue")

set.seed(144)
KMC = kmeans(limitedV, centers = 5, iter.max = 1000)
str(KMC)

colnames(stocks)[125] <- "KMC"

stocks <- cbind(stocks, KMC$cluster)
table(stocks$cutFive, stocks$KMC)

subset(stocks, stocks$stock_symbol %in% c("AAPL", "AMZN", "MSFT", "TROW"))
