library(tidytext)
library(tidyverse)


oli1 = readLines(file.choose())   # read oliver twist.txt

oli2 = oli1 %>%
        unnest_tokens(word,text)


oli2 %>%
       count(word, sort=TRUE) %>%
       filter(n>100) %>%
       anti_join(stop_words) %>%
       mutate(word = reorder(word, n)) %>%
       ggplot(aes(word, n)) +
       geom_co(fill="blue") +
       xlab(NULL) +
       coord_flip() +
       theme_bw() +
       labs(y="word fz")       




##### count vectorization

install.packages("superml")

library(superml)
#> Loading required package: R6

# should be a vector of texts
sents <-  c('i am going home and home',
          'where are you going.? //// ',
          'how does it work',
          'transform your work and go work again',
          'home is where you go from to work')

# generate more sentences
n <- 10
sents <- rep(sents, n) 
length(sents)


#For sample, we’ve generated 50 documents. Let’s create the features now. For ease, superml uses the similar API layout as python scikit-learn.

# initialise the class
cfv <- CountVectorizer$new(max_features = 10, remove_stopwords = FALSE)

# generate the matrix
cf_mat <- cfv$fit_transform(sents)

head(cf_mat, 3)

#Now, let’s generate the matrix using its ngram_range features.

# initialise the class
cfv <- CountVectorizer$new(max_features = 10, remove_stopwords = FALSE, ngram_range = c(1, 3))

# generate the matrix
cf_mat <- cfv$fit_transform(sents)

head(cf_mat, 3)

## example2

sents = c('i am alone in dark.','mother_mary a lot',
          'alone in the dark?', 'many mothers in the lot....')
cv = CountVectorizer$new(min_df=0.1)
cv$fit(sents)


CountVectorizer$fit_transform(sentences)

### usage for machine learning

ngram_range = c(1,3) #set the lower and higher range respectively of the resulting ngram tokens

library(data.table)
library(superml)

# use sents from above
sents <-  c('i am going home and home',
          'where are you going.? //// ',
          'how does it work',
          'transform your work and go work again',
          'home is where you go from to work',
          'how does it work')

# create dummy data
train <- data.table(text = sents, target = rep(c(0,1), 3))
test <- data.table(text = sample(sents), target = rep(c(0,1), 3))

head(train,3)

head(test,3)

#Now, we generate features for train-test data:

# initialise the class
cfv <- CountVectorizer$new(max_features = 12, remove_stopwords = FALSE, ngram_range = c(1,3))

# we fit on train data
cfv$fit(train$text)

train_cf_features <- cfv$transform(train$text)
test_cf_features <- cfv$transform(test$text)

dim(train_cf_features)


dim(test_cf_features)


head(train_cf_features, 3)

head(test_cf_features, 3)


#to train a machine learning model on this, you can simply do:


# ensure the input to classifier is a data.table or data.frame object
x_train <- data.table(cbind(train_cf_features, target = train$target))
x_test <- data.table(test_cf_features)


xgb <- RFTrainer$new(n_estimators = 10)
xgb$fit(x_train, "target")

predictions <- xgb$predict(x_test)
predictions


#########

library(text2vec)

