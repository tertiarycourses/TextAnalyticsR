

####### jacaard similarity & hashing

library("textreuse")
library("dplyr")

dir <- system.file("extdata/ats/", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir, tokenizer = tokenize_ngrams, n = 5,
                          keep_tokens = TRUE)
corpus


names(corpus)
doc <- corpus[["lifeofrevrichard00baxt"]]
tokens(doc)[200:210]


# We can then use comparison functions to determine how similar the documents are. 
# Here we use the Jaccard similarity function (which returns a ratio between 0 and 1), 
# though the package implements several other similarity measures. 
# (The Jaccard similarity is defined as the intersection of the sets divided by the union of the sets.)


jaccard_similarity(corpus[["remember00palm"]],
                   corpus[["remembermeorholy00palm"]])

jaccard_similarity(corpus[["lifeofrevrichard00baxt"]],
                   corpus[["remembermeorholy00palm"]]) 


#the pairwise comparison function, which finds all the combination of documents in the corpus 
# and compares them using a function that you specify

pairwise_compare(corpus, jaccard_similarity) %>% 
  round(3) %>% 
  pairwise_candidates() %>% 
  arrange(desc(score))



### redo on dictators

dir = choose.dir()

corpus = TextReuseCorpus(dir = dir, 
                         meta = list(title = "Dictators"),
                         tokenizer = tokenize_ngrams, n = 7)


names(corpus)
doc <- corpus[["donaldtrump"]]
tokens(doc)[200:210]

jaccard_similarity(corpus[["donaldtrump"]],
                   corpus[["stalin"]])

jaccard_similarity(corpus[["donaldtrump"]],
                   corpus[["hitler"]])

pairwise_compare(corpus, jaccard_similarity) %>% 
  round(3) %>% 
  pairwise_candidates() %>% 
  arrange(desc(score))


### Hashing
# Hashing is a technique for mapping arbitrary values to compact, 
# uniform representations

song <- tokenize_words("the answer is blowin' in the wind")
hashed_tokens <- hash_string(song)
hashed_tokens

hash_string("the") 
hash_string("the") == hashed_tokens[1]  # returns TRUE > exact match

hash_string("The") == hashed_tokens[1]  # returns FALSE > case sensitive


min(hashed_tokens)
song[which.min(hashed_tokens)]


song_mod <- tokenize_words("the answer is blowin' in the breeze")
jaccard_similarity(song, song_mod)


### locality-sensitive hashing (LSH)

minhash <- minhash_generator(n = 20, seed = 12231)
minhash(song) 