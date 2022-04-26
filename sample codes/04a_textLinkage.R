######### import files

docs= read.csv(file.choose())  # choose sports tweets

str(docs)  
docs$LEAGUE=NULL
docs = as.vector(docs$TWEET)



### POS - PART OF SPEECH

library(tidytext) 

ll=length(docs)
docs_df <- data_frame(line = 1:ll, text = docs)


docs_df %>% unnest_tokens(word,text) %>%
  inner_join(parts_of_speech) %>%
  head(20)


docs_df %>% unnest_tokens(word,text) %>%
  inner_join(parts_of_speech) %>%                   # join POS
  count(pos) %>%                                              # count
  mutate(prop=n/sum(n))



### NER - NAME ENTITY RECOGNITION

MONKEYLEARN_KEY="92ea330047a7afe2dabd64fe87e7e22d922f302e"

#devtools::install_github("ropensci/monkeylearn")
library("monkeylearn")

#you can get the code for the entirty extractor from the website
#below is one example

#browseURL("https://app.monkeylearn.com/main/extractors/ex_isnnZRbS/tab/api-tab/")

text <- "In the 19th century, the major European powers had gone to great lengths to maintain a balance of power throughout Europe, 
resulting in the existence of a complex network of political and military alliances throughout the continent by 1900.
[7] These had started in 1815, with the Holy Alliance between Prussia, Russia, and Austria. 
Then, in October 1873, German Chancellor Otto von Bismarck negotiated the League of the Three Emperors 
(German: Dreikaiserbund) between the monarchs of Austria-Hungary, Russia and Germany."

michael="Michael Joseph Jackson (August 29, 1958 - June 25, 2009) was an American singer, songwriter, and dancer. Dubbed the 'King of Pop', he is regarded as one of the most significant cultural figures of the 20th century and one of the greatest entertainers. Jackson's contributions to music, dance, and fashion, along with his publicized personal life, made him a global figure in popular culture for over four decades.
The eighth child of the Jackson family, Michael made his professional debut in 1964 with his elder brothers Jackie, Tito, Jermaine, and Marlon as a member of the Jackson 5. He began his solo career in 1971 while at Motown Records, and in the early 1980s, became a dominant figure in popular music. His music videos, including those for'Beat It', 'Billie Jean', and 'Thriller' from his 1982 album Thriller, are credited with breaking racial barriers and transforming the medium into an art form and promotional tool. Their popularity helped bring the television channel MTV to fame. Bad (1987) was the first album to produce five US Billboard Hot 100 number-one singles.[nb 1] He continued to innovate throughout the 1990s with videos such as 'Black or White' and 'Scream', and forged a reputation as a touring artist. Through stage and video performances, Jackson popularized complicated dance techniques such as the robot and the moonwalk, to which he gave the name. His sound and style have influenced artists of various genres.
Jackson is one of the best-selling music artists of all time, with estimated sales of over 350 million records worldwide;[nb 2] Thriller is the best-selling album of all time, with estimated sales of 66 million copies worldwide. His other albums, including Off the Wall (1979), Bad (1987), Dangerous (1991), and HIStory (1995), also rank among the world's best-selling. He won hundreds of awards (more than any other artist in the history of popular music), has been inducted into the Rock and Roll Hall of Fame twice, and is the only pop or rock artist to have been inducted into the Dance Hall of Fame. His other achievements include Guinness world records (including the Most Successful Entertainer of All Time), 15 Grammy Awards (including the Legend and Lifetime Achievement awards), 26 American Music Awards (more than any other artist), and 13 number-one US singles (more than any other male artist in the Hot 100 era). Jackson was the first artist to have a top ten single in the Billboard Hot 100 in five different decades. In 2016, his estate earned $825 million, the highest yearly amount for a celebrity ever recorded by Forbes.
In the late 1980s, Jackson became a figure of controversy for his changing appearance, relationships, behavior and lifestyle. In 1993, he was accused of sexually abusing the child of a family friend. The accusation was settled out of court. In 2005, he was tried and acquitted of further child sexual abuse allegations and several other charges. In 2009, while preparing for a series of comeback concerts, This Is It, Jackson died from an overdose of sedatives administered by his personal physician, Conrad Murray. Jackson's fans around the world expressed their grief, and his public memorial service was broadcast live. The 2019 documentary Leaving Neverland detailed renewed child sexual abuse allegations and led to an international backlash against Jackson."




#text=readLines(file.choose())
#text=text[1:10]

### location
output = monkey_extract(input = michael, key=MONKEYLEARN_KEY, extractor_id = "ex_isnnZRbS")

output

attr(output, "headers")


#### key words

# If the documentation of the extractor you use states it has parameters,
#  you can pass them as a named list, see below.

text <- "A panel of Goldman Sachs employees spent a recent Tuesday night at the
Columbia University faculty club trying to convince a packed room of potential
recruits that Wall Street, not Silicon Valley, was the place to be for computer
scientists.\n\n The Goldman employees knew they had an uphill battle. They were
 fighting against perceptions of Wall Street as boring and regulation-bound and
 Silicon Valley as the promised land of flip-flops, beanbag chairs and million-dollar
  stock options.\n\n Their argument to the room of technologically inclined students
  was that Wall Street was where they could find far more challenging, diverse and,
   yes, lucrative jobs working on some of the worlds most difficult technical problems.\n\n
   Whereas in other opportunities you might be considering, it is working one type of data
   or one type of application, we deal in hundreds of products in hundreds of markets, with
    thousands or tens of thousands of clients, every day, millions of times of day worldwide,
     Afsheen Afshar, a managing director at Goldman Sachs, told the students."


# You can find extractors and their IDs


output = monkey_extract(text, key=MONKEYLEARN_KEY,
                        extractor_id = "ex_y7BPYzNG",
                        params = list(max_keywords = 3))
output




output2 = monkey_extract(text, key=MONKEYLEARN_KEY,
                         extractor_id = "ex_y7BPYzNG",
                         params = list(max_keywords = 1))
output2

attr(output2, "headers")




####### text link analysis


#----------------------- BIGRAMS


doc_bigrams <- docs_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

doc_bigrams


bigram_counts = doc_bigrams %>%
  count(bigram, sort = TRUE)

bigram_counts = bigram_counts %>% filter(n<20)


#### bigram graph

library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 3) %>%
  graph_from_data_frame()

bigram_graph


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#### bigram wordcloud

library(wordcloud)

# plot wordcloud 2 (uses data frame)

wordcloud(bigram_counts$bigram, 
          bigram_counts$n, 
          random.order=FALSE, 
          colors=brewer.pal(6, "Dark2"),
          min.freq=5, scale=c(4,.2),rot.per=.15,max.words=100)



# ----------------- COSINE SIMILARITY

# which words are associated with sport (taken fromsports tweets)

tdm0 %>% findAssocs("sport", 0.2)


#######

textTdm = docs_dtm

# frequent terms
findFreqTerms(textTdm, lowfreq=11) # term at least 5 times

# associations
findAssocs(textTdm, "german", 0.1)
# search term 'german' from our above list with correlation of 0.3



############## Herarical clustering of terms

## clustering of terms remove sparse terms
m2 <- tdm0 %>% removeSparseTerms(sparse = 0.95) %>% as.matrix()
# calculate distance matrix
dist.matrix <- m2 %>% scale() %>% dist()
# hierarchical clustering
fit <- dist.matrix %>% hclust(method = "ward")


plot(fit)
fit %>% rect.hclust(k = 6) # cut tree into 6 clusters
groups <- fit %>% cutree(k = 6)



######## word pairing


library(janeaustenr)
library(widyr)

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# count words co-occuring within sections

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs



#The pairwise_cor() function in widyr lets us find the phi coefficient between words based 
#on how often they appear in the same section

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

library(ggraph)
library(igraph)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()




