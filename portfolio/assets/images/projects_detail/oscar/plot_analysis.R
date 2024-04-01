library(data.table)
library(quanteda)
library(topicmodels)
library(stm)
library(tidytext)
library(wordcloud)

# topic modeling
# Read the data
DIR <- "/Users/yeonieheo/Desktop/"
data <- fread(paste0(DIR, "oscar_plot.csv"), header=T)

data <- dt$Plot
data <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", data)
data <- gsub("\\b\\w{1,2}\\s", "", data)

# 1) EXPECTED TOPIC PROPORTION
# ========================================================================
# Convert the data into document-term matrix, the basic input form of majority of
# topic model tools in R; using quanteda is very useful for this!
data_dfm <- dfm(data, tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE,
                stem=TRUE, remove_symbols=TRUE, remove=stopwords("SMART"))

data_dfm_trim <- dfm_trim(data_dfm, min_docfreq = 0.10, max_docfreq = 0.90, docfreq_type = "prop") # probability is the standard of trimming in this case
data_dfm_trim

# Find the sum of words in each doc to remove docs without non-zero entry as a 
# result of the pre-processing
rowTotals <- apply(data_dfm_trim , 1, sum) # check how many words each document has
dtm_for_model <- data_dfm_trim[rowTotals > 0, ] # create dtm with documents that have at least one word 

# Choosing optimal k
# The stm package has a useful function called searchK which allows the user to
# specify a range of values for k, runs STM models for each value of 'k', and then
# outputs multiple goodness-of-fit measures that are very useful in identifying
# a range of values of k that provide the best fit for the data. The syntax of
# this function is very similar to the stm function, except that the user specifies
# a range for k as one of the arguments. In the code below, we search all values of
# k between 5 and 30 with an interval of 5.
findingk <- searchK(data_stm_trim$documents, data_stm_trim$vocab,
                    K=seq(from=5, to=10, by=1), verbose=FALSE)
plot(findingk)

# Set k
topic_count <- 5

# Run LDA using Gibbs sampling
plot_lda <- LDA(dtm_for_model, k=topic_count, method="Gibbs", control=list(seed=1))
plot_lda

# Docs to topics
plot_lda_topics = topics(plot_lda)
plot_lda_topics

# Top 15 terms in each topic
plot_lda_terms <- as.matrix(terms(plot_lda, 15))
plot_lda_terms


# more examples HERE: https://warin.ca/shiny/stm/!
data_stm_trim <- convert(data_dfm_trim, to="stm") # stm uses slightly different documentation format

hiphop_stm <- stm(data_stm_trim$documents, data_stm_trim$vocab, K=topic_count,
                  data=data_stm_trim$meta, init.type="Spectral") # verbose=FALSE

plot(hiphop_stm, type="summary")

# 2) EXPECTED TOPIC PROPORTION BY RELEASE ERA
# ========================================================================
DIR <- "/Users/yeonieheo/Desktop/"
data <- fread(paste0(DIR, "oscar_plot.csv"), header=T)

# Bin release year into era to use it as a covariate
data$release_era <- as.numeric(data$Year)
data$release_era <- ifelse(((data$Year>=1980)&(data$Year<=1989)),
                           "1980s", data$Year)
data$release_era <- ifelse(((data$Year>=1990)&(data$Year<=1999)),
                           "1990s", data$Year)
data$release_era <- ifelse(((data$Year>=2000)&(data$Year<=2009)),
                           "2000s", data$Year)
data$release_era <- ifelse(((data$Year>=2010)&(data$Year<=2019)), 
                           "2010s", data$Year)
data$release_era <- ifelse(data$Year>=2020, "2020s", data$Year)

# Extract only those columns needed in the analysis
data <- as.data.frame(data[,c("Plot","release_era")])

data$plot <- gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+", "", data$Plot)
data$plot <- gsub("\\b\\w{1,2}\\s", "", data$Plot)
data_dfm <- dfm(data$Plot,
                tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE,
                stem=TRUE, remove_symbols=TRUE, remove=stopwords(source="smart"))
data_dfm_trim <- dfm_trim(data_dfm, min_docfreq=0.10, max_docfreq=0.90, docfreq_type="prop") # min 5% / max 95%
data_stm_trim <- convert(data_dfm_trim, to="stm")

# produce another data stm for documents that aren't dropped (non-empty documents)
row_index <- ls(data_stm_trim$documents)
row_index <- as.numeric(gsub("text", "", row_index))
data_non_empty <- data[row_index, ]
data_non_empty$index <- as.numeric(row.names(data_non_empty))
data_non_empty <- data_non_empty[order(data_non_empty$index), ] # properly order the increasing index of documents

data_stm_trim$meta <- list()
# covariate: release era
data_stm_trim$meta[["release_era"]]=data_non_empty$release_era
data_stm_trim$meta[["release_era_numeric"]]=as.numeric(gsub("s", "", data_non_empty$release_era))

out <- prepDocuments(data_stm_trim$documents, data_stm_trim$vocab, data_stm_trim$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Again, the key innovation of the STM is that it incorporates metadata into 
# the topic modeling framework. In STM, metadata can be entered in the topic
# model in two ways:
# i. Topical prevalence: Metadata covariates for topical prevalence allow the
# observed metadata to affect the frequency with which a topic is discussed.
# ii. Topical content: Covariates in topical content allow the observed metadata
# to affect the word rate use within a given topic – that is, how a particular
# topic is discussed.

topic_count <- 5

# Second, specify parameters and run it!
# without "max.em.its=75" function, this code runs until it converges.
# This way, this code will converge after 75 iterations.
# how to mark the covariate: function "prevalence = ~ release_era_numeric + billboard)
oscar_stm <- stm(documents=out$documents, vocab=out$vocab, data=out$meta, K=topic_count,
                  max.em.its=75, init.type="Spectral", prevalence = ~ release_era_numeric)

plot(oscar_stm, type="summary")

# Let's print out some important words using labelTopics. This function by default
# prints several different types of word profiles, including highest probability words
# and FREX words. FREX weights words by their overall frequency and how exclusive
# they are to the topic (Hausser and Strimmer 2009). Lift weights words by dividing
# by their frequency in other topics, therefore giving higher weight to words that
# appear less frequently in other topics (Taddy 2013). Similar to lift, score divides
# the log frequency of the word in the topic by the log frequency of the word in other topics.
labelTopics(oscar_stm, c(1:5))

# We can have wordcloud as well.
cloud(oscar_stm, topic=1, scale=c(2,.25))

# A comparison between topics is also possible!
plot(oscar_stm, type="perspectives", topics=c(3,4)) # Topics #2 and #3

# Also, we can easily check the distribution of topic relevance over all the topics and docs!
plot(oscar_stm, type="hist")

# When the covariate of interest is binary, or users are interested in a particular contrast,
# the method = "difference" option will plot the change in topic proportion shifting
# from one specific value to another.
plot(prep, covariate="release_era_numeric", method="continuous", topics=c(1:5), model=oscar_stm,
     xlab="Year", printlegend=FALSE, linecol=c("#2c2d4a","royalblue","#f1aa00","forestgreen","#be5d05"))
legend(1988, 0.45, c("Love", "Empire", "Crime", "War", "Family"), lwd=2,
       col=c("#2c2d4a","royalblue","#f1aa00","forestgreen","#be5d05"))