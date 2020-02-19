#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#                  3. Text Preparation and Exploring Document-Features-Matrix (DFM)
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

#install.packages("quanteda")
#install.packages("topicmodels")
#install.packages("stm")
#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("plyr")
library(ggplot2)
library(data.table)
library(plyr)
library(quanteda)
library(topicmodels)
library(stm)

###########################################################
#
#                  3.1. Import the data sets
#
###########################################################

jun_anti_senti <- read_csv("Dara/jun_anti_senti.csv", 
                             col_types = cols(status_id = col_character(), 
                                              user_id = col_character()))
jun_anti_senti$X1 <- NULL

###########################################################
#
#                  3.2. Clean up tweet content
#
###########################################################

#### preprocessing the content of tweets
jun_anti_senti$text[c(1, 2, 3, 5, 12, 2640, 56, 230)]
# clean up the tweet
jun_anti_senti$text <-  gsub("\\s*<\\U\\+\\w+>\\s*|-", "", jun_anti_senti$text) # Remove unicode
jun_anti_senti$text <-  gsub("\\b[A-z]\\b{1}", " ", jun_anti_senti$text) # Remove single letter
jun_anti_senti$text <-  gsub("[ |\t]{2,}", " ", jun_anti_senti$text)  # Remove tabs
jun_anti_senti$text <-  gsub("^ ", "", jun_anti_senti$text)  # Leading blanks
jun_anti_senti$text <-  gsub(" $", "", jun_anti_senti$text)  # Lagging blanks
jun_anti_senti$text <- gsub(" +", " ", jun_anti_senti$text) # General spaces 
jun_anti_senti$text[c(1, 2, 3, 5, 12, 2640, 56, 230)]

###########################################################
#
#                  3.3. Ingest: preparing corpus and dfm
#                  3.3.1: corpus_jun and kwic_jun_anti_senti
#
###########################################################

corpus_jun <- corpus(jun_anti_senti, 
                     docid_field = "id",
                     text_field = "text")
summary(corpus_jun, 5)

# Here, I am able to play with kwic result showing 7 words around the keyword "anti"
wiccorpus_jun <- kwic(corpus_jun, pattern = "anti", window = 7)

# We could save the output
#kwic_nov_SC <- setDT(wiccorpus_jun)
#write.csv(kwic_nov_SC, "kwic_nov_SC.csv")

###########################################################
#
#                  3.3. Ingest: preparing corpus and dfm
#                  3.3.2: dfm_jun and trimmed_dfm_jun
#
###########################################################

dfm_jun <- dfm(corpus_jun, tolower = TRUE, 
               remove = stopwords("english"), stem = FALSE,
               remove_punct = TRUE)
# set "stem=FALSE" to remain all the possible features in the dataset.  

# wordcloud for words with 2+ frequency
set.seed(1000)
textplot_wordcloud(dfm_jun, min_count = 2, 
                   random_order = FALSE, fixed_aspect = TRUE,
                   rotation = FALSE,
                   color = RColorBrewer::brewer.pal(10, "Dark2"))

# trimming if needed:
# select terms with doc_freq >= 2
# select term with frequency above 10% and below 90%
#trimmed_jun_anti_senti <- dfm_trim(dfm_jun, 
#min_docfreq = 2,
#docfreq_type = "count",
#max_termfreq = .9,
#termfreq_type = "quantile")
#trimmed_jun_anti_senti

# wordcloud for words with 2+ frequency
#set.seed(1000)
#textplot_wordcloud(trimmed_jun_anti_senti, min_count = 2, 
# random_order = FALSE, fixed_aspect = TRUE,
# rotation = FALSE,
# color = RColorBrewer::brewer.pal(10, "Dark2"))

#topfeatures_jun_anti_senti <- setDT(as.data.frame(topfeatures(trimmed_jun_anti_senti,
#n = 30, decreasing = TRUE,
# scheme = "count")), 
#   keep.rownames = TRUE)[]
#colnames(topfeatures_jun_anti_senti) <- c("Top 30 Words", "Frequency")
#write_csv(topfeatures_jun_anti_senti, "topfeatures_jun_anti_senti.csv")


###########################################################
#
#                  3.4. Extract: exploring hashtag
#                  3.4.1 hashtag_dfm
#
###########################################################


# keep only hashtags
hashtag_dfm <- dfm(corpus_jun, 
                   select = "#*", 
                   remove_twitter = FALSE)

hashtag_jun <- setDT(as.data.frame(colSums(hashtag_dfm)), keep.rownames = TRUE)[]
colnames(hashtag_jun) <- c("hashtag", "frequency")
hashtag_jun <- hashtag_jun[order(hashtag_jun$frequency, decreasing = TRUE),]  

# delete the empty hashtag as "#" if need
#hashtag_jun <- hashtag_jun[-44,]
# Save the output if needed
#write_csv(hashtag_jun, "hashtag_nov_SC.csv")

# visualize the hashtage frequency >20
textplot_wordcloud(hashtag_dfm, min_count = 20, rotation = FALSE, 
                   random_order = FALSE, fixed_aspect = TRUE,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


###########################################################
#
#                  3.4. Extract: exploring hashtag
#                  3.4.2 Keyness in Sentiment Comparison
#
###########################################################

# create DFM that contains sentiments
corpus_sentiment <-  corpus_subset(corpus_jun, 
                                   sentiment_class %in% c("Negative", "Neutral", "Positive"))
summary(corpus_sentiment, 5)
DFM_sentiment <-  dfm(corpus_jun, groups = "sentiment_class", 
                      remove = stopwords("english"), remove_punct = TRUE)

# compare target (in this case negative) to 
# the rest of DFM (in this case both neutral and positive)
keyness <-  textstat_keyness(DFM_sentiment, target = "Negative", measure = "lr") 
textplot_keyness(keyness, color = c("darkred", "darkblue"), n = 30)
# Save the output if needed
#write_csv(keyness, "keyness_jun_anti_senti.csv")

###########################################################
#
#                  3.4. Extract: exploring hashtag
#                  3.4.3 tag_jun_fcm (feature-occurrence matrix)
#
###########################################################

tag_jun_fcm <- fcm(dfm_jun)
head(tag_jun_fcm)

toptag <- names(topfeatures(hashtag_dfm, 50))

topgat_fcm <- fcm_select(tag_jun_fcm, pattern = toptag)

set.seed(18481)
textplot_network(topgat_fcm, edge_alpha = 0.5, edge_size = 5, 
                 vertex_color = "#144c73", 
                 vertex_labelcolor = "#4D4D4D", vertex_labelsize = 5)

###########################################################
#
#                  3.5. Extract: exploring user name
#
###########################################################

# Extract most frequently mentioned usernames
user_jun_fcm <- dfm_select(dfm_jun, pattern = "@*")
topuser <- names(topfeatures(user_jun_fcm, 100))
head(topuser)

user_fcm <- fcm(user_jun_fcm)
head(user_fcm)

user_fcm <- fcm_select(user_fcm, pattern = topuser)

set.seed(10081)
textplot_network(user_fcm, edge_color = "#ffa500", edge_alpha = 0.65, edge_size = 5,
                 vertex_color = "#e69500", 
                 vertex_labelcolor = "#4D4D4D", vertex_labelsize = 5)
