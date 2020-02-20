#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#                  4. topic modeling analysis
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

###########################################################
#
#                  4.1. Import the data sets
#
###########################################################

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

# Here, I filter out the neutral sentiment class becuase I am more interested to
# discuss the extreme sentimenet on Hong Kong posts.
jun_anti_senti <- filter(nov_SC_sentiment, nov_SC_sentiment$sentiment_class != "Neutral")

###########################################################
#
#                  4.2. corpus preperation
#
###########################################################

# Reading document and create metadata #
metalist <- c("user_id", "id", "created_at", "user_screen_name", "source", "tweet_type",
              "user_favourites_count","user_followers_count","user_friends_count",
              "user_listed_count", "user_statuses_count", "user_verified",
              "ave_sentiment","sentiment_class","HK_localtime","daycount")
metadata <- jun_anti_senti[metalist]
processed <- textProcessor(documents = jun_anti_senti$text,
                           metadata = metadata,
                           lowercase = TRUE, removepunctuation = TRUE,
                           removestopwords = TRUE, removenumbers = TRUE, 
                           stem = FALSE, wordLengths = c(3, Inf))
# trim the corpus if needed
plotRemoved(documents = processed$documents,
            lower.thresh = seq(from = 10, to = 500, by = 10))

out <- prepDocuments(documents = processed$documents,
                     vocab = processed$vocab,
                     meta = processed$meta,
                     lower.thresh = 200)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

###########################################################
#
#                  4.3. Evaluate: STM model selection and search
#                  4.3.1: Spectral Topic Model Selection
#
###########################################################

#install.packages("Rtsne")
#install.packages("rsvd")
#install.packages("geometry") 
library(Rtsne)
library(rsvd)
library(geometry)

# Topical prevalence captures how much each topic contributes to a document.
# Estimation with topic prevalence parameter.
# Using Spectral to auto select the numbers of topics (k)
auto_k <- stm(documents = out$documents, 
                       vocab = out$vocab, K=0, 
                       prevalence=~ sentiment_class + s(daycount), 
                       data = meta, max.em.its=5,
                       init.type = "Spectral")
auto_k
# A topic model with 64 topics, 437775 documents and a 3287 word dictionary.

plot(auto_k, type="perspectives", topics = c(1,2))

###########################################################
#
#                  4.3. Evaluate: STM model selection and search
#                  4.3.2: Search the best K 
#
###########################################################

# Model search across numbers of topics (k)
storage <- searchK(documents = out$documents,
                   prevalence=~ sentiment_class + s(daycount), 
                   data = meta, vocab = out$vocab,
                   K = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 30))
plot.searchK(storage)

# The more appropriate K is ...

###########################################################
#
#                  4.3. Evaluate: STM model selection and search
#                  4.3.3: Search the best Model with selected k
#
###########################################################

# Model selection for 12 topics
modelsearch_k <- selectModel(documents = out$documents, 
                               vocab = out$vocab, 
                               K = 9, #CHANGE K BASED ON ABOVE STORAGE RESULT
                               prevalence=~ sentiment_class + s(daycount),
                               data = out$meta, 
                               max.em.its = 75,
                               seed = 8458159)
modelsearch_k

# Numerals represent the average for each model, and dots represent topic specific scores.
plotModels(modelsearch_k, pch=c(1,2,3,4,5,6,7,8,9,10), legend.position="bottomleft")

# select the model
selectedmodel <- modelsearch_k$runout[[5]]

# indicator comparison
runout_model <- modelsearch_k$runout[[5]]
semcoh_model <- modelsearch_k$semcoh[[10]]
exclusivity_model <- modelsearch_k$exclusivity[[10]]

plot(runout_model$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

###########################################################
#
#                  4.3. Evaluate: STM model selection and search
#                  4.3.4: Estimate: estimating the stm with pre-defined k
#
###########################################################

stm_definedk <- stm(documents = out$documents, 
                  vocab = out$vocab, 
                  K = 9, #CHANGE K BASED ON ABOVE STORAGE RESULT
                  prevalence=~ sentiment_class + s(daycount),
                  data = out$meta, 
                  max.em.its = 75,
                  init.type = "LDA",
                  seed = 8458159)

plot(stm_definedk$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

###########################################################
#
#                  4.4. Understand: Interpreting the STM
#                  4.1.1: Label Topics
#
###########################################################

# Displaying words associated with topics 
labelTopics(selectedmodel, n = 50)
plot.STM(selectedmodel, n = 16, type = "summary", xlim = c(0, .7))

# Match up with the orginal context of tweet
# original text from jun_anti_senti
text <- jun_anti_senti[-out[["docs.removed"]],]

# For confidential privacy purpose, screenout the user names
text$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+):", "", text$text)   # Remove the "RT"& usernames
text$text <- gsub("\\@[[:alnum:]]*", "", text$text)  # Remove @usernames
text$text <- iconv(text$text, to = "ASCII", sub = " ") 

# documents highly associated with particular topics
par(mfrow = c(3, 3),mar = c(1, 1, 1.5, 1))
thoughts_1 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 1)$docs[[1]]
plotQuote(thoughts_1, width = 60, main = "Topic 1")
thoughts_2 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 2)$docs[[1]]
plotQuote(thoughts_2, width = 60, main = "Topic 2")
thoughts_3 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 3)$docs[[1]]
plotQuote(thoughts_3, width = 60, main = "Topic 3")
thoughts_4 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 4)$docs[[1]]
plotQuote(thoughts_4, width = 60, main = "Topic 4")
thoughts_5 <- findThoughts(selectedmodel, texts = text$text,
                           n = 1, topics = 5)$docs[[1]]
plotQuote(thoughts_5, width = 60, main = "Topic 5")
thoughts_6 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 6)$docs[[1]]
plotQuote(thoughts_6, width = 60, main = "Topic 6")
thoughts_7 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 7)$docs[[1]]
plotQuote(thoughts_7, width = 60, main = "Topic 7")
thoughts_8 <- findThoughts(selectedmodel, texts = text$text,
                           n = 1, topics = 8)$docs[[1]]
plotQuote(thoughts_8, width = 60, main = "Topic 8")
thoughts_9 <- findThoughts(selectedmodel, texts = text$text, 
                           n = 1, topics = 9)$docs[[1]]

###########################################################
#
#                  4.4. Understand: Interpreting the STM
#                  4.1.1: Estimate Effect
#
###########################################################

#Estimating relationships between metadata and topics/topical content
out$meta$sentiment_class <- as.factor(out$meta$sentiment_class)
prep <- estimateEffect(1:9 ~ sentiment_class, 
                       selectedmodel,
                       meta = out$meta, 
                       uncertainty = "Global")

# generate regression table
summary(prep, topics=1)
summary(prep, topics=2)
summary(prep, topics=3)
summary(prep, topics=4)
summary(prep, topics=5)
summary(prep, topics=6)
summary(prep, topics=7)
summary(prep, topics=8)
summary(prep, topics=9)
summary(prep, topics=10)
summary(prep, topics=11)
summary(prep, topics=12)


# metadata/topic relationship visualization
# "pointestimate" plot the marginal topic proportion for each of the levels
custom_topics_labels
plot.estimateEffect(prep,
                    covariate = "sentiment_class",
                    cov.value1= "Postive", cov.value2="Negative", 
                    topics = (1:9),
                    method = "pointestimate",
                    model = selectedmodel,
                    xlim = c(-.7, .7), width = 80,
                    xlab = "Topic Proportions for Each Value of The Covariate",
                    main = "The Effect of Sentiment on Topics, nov_SC_sentiment")

# "difference" plot the change in topic proportion shifting from one specific value to another (binary)
plot.estimateEffect(prep,
                    covariate = "sentiment_class", topics = c(1:9),
                    model = selectedmodel, method = "difference",
                    cov.value1 = "Positive", cov.value2 = "Negative", 
                    xlab = "More Negative ........................  More Positive",
                    main = "Effect of Positive vs. Negative on Topics, nov_SC_sentiment",
                    xlim = c(-.5, .5), width = 80, ci.level = .95)

# "continuous" plot
out$meta$daycount <- as.numeric(out$meta$daycount)
plot.estimateEffect(prep, "daycount", method = "continuous", topics = c(1:12),
                    model = selectedmodel, 
                    printlegend = TRUE, xaxt = "n",xlab = "daycount", 
                    axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)),
                         labels = monthnames))
monthseq <- seq(from = as.Date("2019-06-28"), to = as.Date("2019-09-04"), by = "month")
monthnames <- months(monthseq)

###########################################################
#
#                  4.5. Visualize: Presenting Topics Correlation
#
###########################################################

corr_SC <- topicCorr(selectedmodel)
#install.packages("igraph")
library(igraph)
plot(corr_SC, vertex.color = c("#1F78B4", "#ffa500"), 
     vertex.frame.color = "gray", 
     vertex.label.color = "#4D4D4D",
     vertex.label.cex = 1, vertex.label.dist = 2, 
     edge.cuved = 0.3, set.seed(54681))

plot(corr_SC,vertex.color = c("#1F78B4", "red" ))





