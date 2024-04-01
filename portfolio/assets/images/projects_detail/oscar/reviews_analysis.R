library(data.table)
library(stm)
library(quanteda)
library(wordcloud)

DIR <- "/Users/yeonieheo/Desktop/"
data <- fread(paste0(DIR, "movie_cul_data.csv"), header=T)

data$movie_era <- as.numeric(data$movie)
data$movie_era <- ifelse(((data$movie_era>=1980)&(data$movie_era<=1989)),
                           "1980s", data$movie_era)
data$movie_era <- ifelse(((data$movie_era>=1990)&(data$movie_era<=1999)),
                           "1990s", data$movie_era)
data$movie_era <- ifelse(((data$movie_era>=2000)&(data$movie_era<=2009)),
                           "2000s", data$movie_era)
data$movie_era <- ifelse(((data$movie_era>=2010)&(data$movie_era<=2019)),
                         "2010s", data$movie_era)
data$movie_era <- ifelse(data$movie_era>=2020, "2020s", data$movie_era)

data$review <- gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+", "", data$review)
data$review <- gsub("\\b\\w{1,2}\\s", "", data$review)
data_dfm <- dfm(data$review,
                tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE,
                stem=TRUE, remove_symbols=TRUE, remove=stopwords(source="smart"))
data_dfm_trim <- dfm_trim(data_dfm, min_docfreq=0.05, max_docfreq=0.95, docfreq_type="prop") # min 5% / max 95%
data_stm_trim <- convert(data_dfm_trim, to="stm")

# produce anohter data stm for documents that aren't dropped (non-empty documents)
row_index <- ls(data_stm_trim$documents)
row_index <- as.numeric(gsub("text", "", row_index))
data_non_empty <- data[row_index, ]
data_non_empty$index <- as.numeric(row.names(data_non_empty))
data_non_empty <- data_non_empty[order(data_non_empty$index), ] # properly order the increasing index of documents

data_stm_trim$meta <- list()
# determine covariates
data_stm_trim$meta[["type"]]=data_non_empty$type
data_stm_trim$meta[["movie_era"]]=data_non_empty$movie_era
data_stm_trim$meta[["movie_era_numeric"]]=as.numeric(gsub("s", "", data_non_empty$movie_era))

out <- prepDocuments(data_stm_trim$documents, data_stm_trim$vocab, data_stm_trim$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


topic_count <- 7

movie_stm <- stm(documents=out$documents, vocab=out$vocab, data=out$meta, K=topic_count,
                  max.em.its=75, init.type="Spectral", prevalence = ~ movie_era_numeric + type)

plot(movie_stm, type="summary")


labelTopics(movie_stm, c(1:7))

prep <- estimateEffect(1:7 ~ type, movie_stm, meta=out$meta, uncertainty="Global")
summary(prep, topics=1)

# covariate: type
plot(prep, covariate="type", topics=c(1:7), model=movie_stm, method="difference",
     cov.value1="audience", cov.value2 = "critic",
     xlab="Audience Reviews ... Critic Reviews", xlim=c(-0.1, 0.1),
     labeltype="custom", custom.labels=c("Time went fast", "Enjoy", "Producer", "Actors", "General", "Performance", "Story Line"))

# ========================================================================
# covariate: release era
D
# When the interaction between covariates is the interest... .
prep <- estimateEffect(c(1) ~ movie_era_numeric*type, movie_stm, metadata=out$meta, uncertainty="None")
plot(prep, covariate="movie_era_numeric", model=movie_stm, method="continuous", xlab="movie",
     moderator="type", moderator.value=1, linecol="#f1aa00", printlegend=FALSE)
plot(prep, covariate="movie_era_numeric", model=movie_stm, method="continuous", xlab="movie",
     moderator="type", moderator.value=0, linecol="#be5d05", add=TRUE, printlegend=FALSE)
legend(1996, 0.06, c("Audience Reviews", "Critic Reviews"), lwd=2, col = c("#f1aa00", "#be5d05"))

prep <- estimateEffect(c(5) ~ movie_era_numeric*type, movie_stm, metadata=out$meta, uncertainty="None")
plot(prep, covariate="movie_era_numeric", model=movie_stm, method="continuous", xlab="movie",
     moderator="type", moderator.value=1, linecol="#f1aa00", printlegend=FALSE)
plot(prep, covariate="movie_era_numeric", model=movie_stm, method="continuous", xlab="movie",
     moderator="type", moderator.value=0, linecol="#be5d05", add=TRUE, printlegend=FALSE)
legend(1970, 0.28, c("Audience Reviews", "Critic Reviews"), lwd=2, col = c("#f1aa00", "#be5d05"))

