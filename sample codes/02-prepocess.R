
library(tm)
library(SnowballC)

######### import files

docs= read.csv(file.choose())  # choose sports tweets

str(docs)  
docs$LEAGUE=NULL
docs = as.vector(docs$TWEET)


### remove non english words
docs <- docs[which(!grepl("[^\x01-\x7F]+", docs))]


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
data("crude")
x1=tm_map(x1,stemCompletion(crude))

## VCorpus *


### corpus
x1    # at this stage x1 is a corpus
inspect(x1)
# see content of corpus
x1[[1]]$content

### term document Frequency
tdm0 = TermDocumentMatrix(x1)   # must remove non english words first
inspect(tdm0)


### document term frequency
dtm01 = t(tdm0)
inspect(dtm01)


library(dplyr)
# inspect frequent words
term.freq <- tdm0 %>% findFreqTerms(lowfreq = 10) %>% print


## word cloud
m <- tdm0 %>% as.matrix
# calculate the frequency of words and sort it by frequency
word.freq <- m %>% rowSums() %>% sort(decreasing = T)
# colors
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


########################## convert to tidy form

library(tidytext)

text_df = tidy(tdm0)
text_df


# turning document-feature matrices into a one-token-per-document-per-row table:

text_df_idf <- text_df %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

text_df_idf2 = text_df_idf %>% head(10)


##### visualize in graph

library(ggplot2)

ggplot(text_df_idf2) + aes(x=term, y=tf_idf, fill=document) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_grid(document ~.)


### create a corpus from many documents
docs=Corpus(DirSource(directory=choose.dir()))   # choose your folder



################## using TIDYTEXT PACKAGE

######### new method using tidytext #######

docs=readLines(file.choose())
ll=length(docs)

docs_df <- data_frame(line = 1:ll, text = docs)

docs_df2 = docs_df %>%                         # restart if R crash > clear gc()
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

### visualize top 20 words
docs_df2 %>%  
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%   # we choose the first 20 words
  ggplot()+ aes(x=word, y=n) + 
  geom_bar(stat="identity")


ll3=nrow(docs_df2)     # need to add extra "doc" column
dd2=data.frame(doc=rep(1,ll3))   # coz only 1 document
docs_df3 = cbind(dd2,docs_df2)

## document frequency matrix  
docs_dfm= docs_df3 %>%
  cast_dfm(doc, word, line)   # follow the data.frame headers
# follow this order

# document term matrix
docs_dtm = docs_df3 %>%
  cast_dtm(doc, word, line)

# term document matrix
docs_tdm = docs_df3 %>%
  cast_dtm(doc, word, line)

