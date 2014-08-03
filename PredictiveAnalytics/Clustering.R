library(caret)
library(flexclust)

kos <- read.csv("./data/dailykos.csv")
dist <- dist(kos[2:1546], method = "euclidean")
kosClust <- hclust(dist, method = "ward") 
plot(kosClust)

clusterGroups = cutree(kosClust, k = 7)

cluster1 = subset(kos, clusterGroups==1)
cluster2 = subset(kos, clusterGroups==2)
cluster3 = subset(kos, clusterGroups==3)
cluster4 = subset(kos, clusterGroups==4)
cluster5 = subset(kos, clusterGroups==5)
cluster6 = subset(kos, clusterGroups==6)
cluster7 = subset(kos, clusterGroups==7)

nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
nrow(cluster5)
nrow(cluster6)
nrow(cluster7)

tail(sort(colMeans(cluster1[-1])))
tail(sort(colMeans(cluster2[-1])))
tail(sort(colMeans(cluster3[-1])))
tail(sort(colMeans(cluster4[-1])))
tail(sort(colMeans(cluster5[-1])))
tail(sort(colMeans(cluster6[-1])))
tail(sort(colMeans(cluster7[-1])))

set.seed(1000)
KMC = kmeans(kos[2:1546], centers = 7, iter.max = 1000)

KmeansCluster = split(kos, KMC$cluster)
tail(sort(colMeans(KmeansCluster[[1]][-1])))
tail(sort(colMeans(KmeansCluster[[2]][-1])))
tail(sort(colMeans(KmeansCluster[[3]][-1])))
tail(sort(colMeans(KmeansCluster[[4]][-1])))
tail(sort(colMeans(KmeansCluster[[5]][-1])))
tail(sort(colMeans(KmeansCluster[[6]][-1])))
tail(sort(colMeans(KmeansCluster[[7]][-1])))
# or 
lapply(KmeansCluster, function(vec) tail(sort(colMeans(vec))))

table(clusterGroups, KMC$cluster)
prop.table(table(clusterGroups, KMC$cluster),2)

###########################################
# Airlines
###########################################

airlines <- read.csv("./data/AirlinesCluster.csv")

preproc = preProcess(airlines)
# perform normalization
airlinesNorm = predict(preproc, airlines)

dist2 <- dist(airlinesNorm, method = "euclidean")
airlinesClust <- hclust(dist2, method = "ward.D") 
plot(airlinesClust)
rect.hclust(airlinesClust, k = 5, border = "red")

clusterGroups2 = cutree(airlinesClust, k = 5)
table(clusterGroups2)

for (i in 1:7 ){
    print (max(tapply(airlines[,i], clusterGroups2, mean)))
    print (which.max(tapply(airlines[,i], clusterGroups2, mean)))
}

for (i in 1:7 ){
    print (tapply(airlines[,i], clusterGroups2, mean))
}

# K-means clustering
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(KMC)
table(KMC$size>1000)


###########################################
# Medicare Claims
###########################################

claims <- read.csv("./data/reimbursement.csv")
table(rowSums(claims[2:12])==0)
280427 / (280427+177578)

claims$reimbursement2008 = log(claims$reimbursement2008+1)
claims$reimbursement2009 = log(claims$reimbursement2009+1)

set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]

train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)

set.seed(144)
km <- kmeans(train.norm , centers=3)
km$centers

#  use the flexclust package to obtain training set and testing set cluster 
# assignments for our observations
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
table(cluster.test)

train1 <- subset(train, cluster.train==1)
train2 <- subset(train, cluster.train==2)
train3 <- subset(train, cluster.train==3)

test1 <- subset(test, cluster.test==1)
test2 <- subset(test, cluster.test==2)
test3 <- subset(test, cluster.test==3)

mean(train1$reimbursement2009)
mean(train2$reimbursement2009)
mean(train3$reimbursement2009)

lm1 <- lm(reimbursement2009 ~., data=train1)
lm2 <- lm(reimbursement2009 ~., data=train2)
lm3 <- lm(reimbursement2009 ~., data=train3)

pred.test1 = predict(lm1, newdata=test1)
pred.test2 = predict(lm2, newdata=test2)
pred.test3 = predict(lm3, newdata=test3)

mean(pred.test1)
mean(pred.test2)
mean(pred.test3)

SSE1 <- sum((pred.test1-test1$reimbursement2009)^2)
RMSE1 <- sqrt(SSE1/nrow(test1))

SSE2 <- sum((pred.test2-test2$reimbursement2009)^2)
RMSE2 <- sqrt(SSE2/nrow(test2))

SSE3 <- sum((pred.test3-test3$reimbursement2009)^2)
RMSE3 <- sqrt(SSE3/nrow(test3))

# or
sqrt(mean((pred.test1 - test1$reimbursement2009)^2))


all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
sqrt(mean((all.predictions - all.outcomes)^2))
