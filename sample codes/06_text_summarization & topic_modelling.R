
### text summarization with LSA

hitler = readLines(file.choose())

library(LSAfun)

genericSummary(hitler,k=1)   # k is number of sentences

#### topic modelling


library(tm)    
library(stm)
library(quanteda)
library(ggplot2)
library(tidytext)
library(dplyr)
library(jpeg)
library(reshape2)
library(topicmodels)


#### choose the airline sentiments

air = read.csv(file.choose(), stringsAsFactors=FALSE)
str(air)

air = air[,c("tweet_created","airline_sentiment","text")]
air2 = air$text

### remove non english words
docs <- air2[which(!grepl("[^\x01-\x7F]+", air2))]


tweet.removeURL = function(x) gsub("http[^[:space:]]","",x)
tweet.removeATUser = function(x) gsub("@[a-x,A-Z]","",x)
tweet.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}","", x, perl=TRUE)
tweet.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]","",x)

#docs=readLines(file.choose())
x1 = Corpus(VectorSource(docs))  	# Constructs a source for a vector as input

#rm(docs)
gc()



x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, content_transformer(tolower))		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents

####3 words that you want to remove

#word2=c("coronavirus","covid-19","virus","corona","covid_19",
#        "t.co","https","covid","19","covid19")

#funny_words = c("netflix","tcojtohdzgx","tcojtohdzgx","tcocqaofkgac","via",
#                "tcogwodbmxyuk","tcobqfjmo","tcoihiacnxu","tconvuytyfv", "tcovauricmj","tconqshurogit")


#x1 = tm_map(x1, removeWords, c(stopwords('english'),word2))
#x1 = tm_map(x1, removeWords, c(funny_words))



x1 = tm_map(x1, content_transformer(tweet.removeURL))
x1 = tm_map(x1, content_transformer(tweet.removeATUser))
x1 = tm_map(x1, content_transformer(tweet.removeEmoji))
x1 = tm_map(x1, content_transformer(tweet.removeSpecialChar))

##### remove stopwords

x1 = tm_map(x1, removeWords, c(stopwords('english')))


##### stemming & lemmatization

x1=tm_map(x1, stemDocument)

## VCorpus *


### corpus
x1    # at this stage x1 is a corpus
inspect(x1)
# see content of corpus
x1[[1]]$content


########## using tidymodels package

### term document Frequency
tdm0 = TermDocumentMatrix(x1)   # must remove non english words first
inspect(tdm0)


### document term frequency
dtm01 = t(tdm0)
inspect(dtm01)


##After creating a textmatrix, we can run the LDA function (and by default it searches for an appropriate number of topics)


lda <- LDA(dtm01, k = 4) # find 4 topics
term <- terms(lda, 5) # first 5 terms of every topic
term <- apply(term, MARGIN = 2, paste, collapse = ", ") %>% print

library(lubridate)

rdm.topics <- topics(lda) # 1st topic identified for every document (tweet)



##=== time series analysis

### topics across time


dater = air$tweet_created
dater = dater[1:length(rdm.topics)]

rdm.topics <- data.frame(date=lubridate::mdy_hm(dater),
                         topic=rdm.topics)


ggplot(rdm.topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")


### get the sentiments of the tweets


# sentiment analysis
library(tidytext)

texter = air$text

text_df = data.frame(line = 1:length(texter), 
                     text = texter)

nrc2 <- get_sentiments("nrc")

text_df %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(nrc2) %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(20) %>%
  ggplot() + aes(word, n, fill=sentiment) + geom_bar(stat="identity")


## positive & negative sentiment cloud


library(RColorBrewer)
text_df %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(nrc2) %>%
  filter(sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(color = brewer.pal(8, "Dark2"), title.size = 1, scale = c(3,  0.3), random.order = FALSE,  max.words = 100)

### emotions sentiment cloud

library(RColorBrewer)
text_df %>% unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(nrc2) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(color = brewer.pal(8, "Dark2"), title.size = 1, scale = c(3,  0.3), random.order = FALSE,  max.words = 100)


######## using qunateda package


### document feature matrix
corp_x1 <- corpus(x1)   # using qunateda package 
dfmat_x1 <- dfm(corp_x1)


#### breakup into 3 topics # this will take about 10 minutes


library(stm)
my_lda_fit3 <- stm(dfmat_x1, K = 3, verbose = FALSE)
plot(my_lda_fit3)


#plot(topic_mod)

td_beta = tidy(topic_mod)


### visualize topics and key words
td_beta %>% group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap (~ topic, scales = "free") + 
  coord_flip()



#### comaprison cloud
td_beta %>%
  #mutate(topic = paste("topic", topic)) %>%
  acast(term ~ topic, value.var = "beta", fill = 0) %>%
  comparison.cloud(max.words = 500)


######## find tweets that match our key words


### choose words from topic 1
toMatch <- c("thank","help","call")
matches_nrow = unique(grep(paste(toMatch,collapse="|"), air$text))
text_df_matches = air[matches_nrow, ]
View(text_df_matches)

###3 using model from previous sentiment classification

corpus2 = Corpus(VectorSource(text_df_matches$text))
corpus2 = tm_map(corpus2, tolower)
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, stemDocument)

dtm2 = DocumentTermMatrix(corpus2)
#spdtm2 = removeSparseTerms(dtm2, 0.95) # remove sparse temrms
airSparse2 = as.data.frame(as.matrix(dtm2))
colnames(airSparse2) = make.names(colnames(airSparse2))
sort(colSums(airSparse2))



predTestLog = predict(airLog, airSparse2, type="response")
table(predTestLog >= 0.00001 & predTestLog <= 0.99999)
table(text_df_matches$airline_sentiment, predTestLog > 0.5)

text_dfp1 = ifelse(text_df_matches$airline_sentiment=="positive",1,0)
pred_dfp2 = ifelse(predTestLog > 0.5,1,0)

MLmetrics::Accuracy(text_dfp1, pred_dfp2)


###3 can repeat with topic 2 and topic 3
