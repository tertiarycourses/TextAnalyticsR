library("tm")
library("SnowballC")
library("e1071")
library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")


#### choose the airline sentiments

air = read.csv(file.choose(), stringsAsFactors=FALSE)
str(air)

air = air[,c("text","airline_sentiment")]
table(air$airline_sentiment)


air = air %>% filter(airline_sentiment !="neutral")
names(air) = c("Text","Type")


# creating the corpus
corpus = Corpus(VectorSource(air$Text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

# create document term matrix
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.995) # remove sparse temrms
airSparse = as.data.frame(as.matrix(spdtm))
colnames(airSparse) = make.names(colnames(airSparse))
sort(colSums(airSparse))

# adding the variable
airSparse$Type = air$Type
sort(colSums(subset(airSparse, Type == 0)))
airSparse$Type = as.factor(airSparse$Type)

# building the model
spl = sample.split(airSparse$Type, 0.7)
train = subset(airSparse, spl == TRUE)
test = subset(airSparse, spl == FALSE)

# log Regression model
airLog = glm(Type~., data=train, family="binomial")

# decision tree model
airCART = rpart(Type~., data=train, method="class")
prp(airCART)

# random forest model ### this will take some time
airRF = randomForest(Type~., data=train)

# naiveBayes

airNB= naiveBayes(train, train$Type)


### predictions

##logResgression
predTestLog = predict(airLog, test, type="response")
table(predTestLog >= 0.00001 & predTestLog <= 0.99999)
table(test$Type, predTestLog > 0.5)
predictionTestLog = prediction(predTestLog, test$Type)
as.numeric(performance(predictionTestLog, "auc")@y.values)


### naiveBayes
predTrainNB = predict(airNB, test, type="class")
predTrainNB=ifelse(predTrainNB=="air",1,0)
table(test$Type, predTrainNB > 0.5)
MLmetrics::Accuracy(predTestNB, test$Type)






### create your own table of airline comments and classify as air/5ham  > make sure column names are the same

air2=data.frame(Text=c("i got a free holiday by travelling",
                         "excellent and fiendly service",
                         "this airline is always late"))

corpus2 = Corpus(VectorSource(air2$Text))
corpus2 = tm_map(corpus2, tolower)
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, stemDocument)

dtm2 = DocumentTermMatrix(corpus2)
#spdtm2 = removeSparseTerms(dtm2, 0.95) # remove sparse temrms
airSparse2 = as.data.frame(as.matrix(dtm2))
colnames(airSparse2) = make.names(colnames(airSparse2))
sort(colSums(airSparse2))



predict(airNB, airSparse2, type="class")
