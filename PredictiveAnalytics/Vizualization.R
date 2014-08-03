library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = "white", color = "blue") + coord_map("mercator")

polling <- read.csv("./data/PollingImputed.csv")

Train = subset(polling, polling$Year %in% c(2004, 2008))
Test = subset(polling, polling$Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
    geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
    geom_polygon(color = "black", alpha=0.3) + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                        labels = c("Democrat", "Republican"), name = "Prediction 2012")


##########################
# Parole
########################

parole <- read.csv("./data/parole.csv")

parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

nrow(subset(parole, parole$male ==0 & parole$violator==1)) /
    nrow(subset(parole, parole$violator==1))

table(parole$state, parole$crime)

### Age of the parolees

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")

# plotes on top of each other vs next to each other
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

# stacked vs overlayed
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age, fill = male)) + 
    geom_histogram(binwidth = 5,position="identity", alpha=0.5)


### Time Served

ggplot(data = parole, aes(x = time.served)) + 
    geom_histogram(binwidth=1, color="black", fill="turquoise4") +
    facet_grid(.~crime)

ggplot(data = parole, aes(x = time.served, fill=crime)) + 
    geom_histogram(binwidth=1, color="black",
                   position="identity", alpha=0.5)


##########################
# Social Network
########################

library(igraph)
library(rgl)

edges <- read.csv("./data/edges.csv")
users <- read.csv("./data/users.csv")

table(users$school, users$locale)
table(users$gender, users$school)

g <- graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sort(degree(g))
table(degree(g) >= 10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

sort(V(g)$size)
summary(degree(g))

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
V(g)$color[V(g)$school == "AB"] = "green"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "grey"
plot(g, vertex.label=NA, edge.width = 3)


rglplot(g, vertex.label=NA)


##########################
# Word Cloud
########################

library(tm)
library(wordcloud)

tweets <- read.csv("./data/tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

frequencies = DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))

wordcloud(names(allTweets),colSums(allTweets), 
          scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.1, use.r.layout=FALSE, colors=brewer.pal(4, "YlOrRd"))

wordcloud(names(allTweets),colSums(allTweets), 
          scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.1, use.r.layout=FALSE, 
          colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)],
          ordered.colors=FALSE)

?brewer.pal
display.brewer.all()