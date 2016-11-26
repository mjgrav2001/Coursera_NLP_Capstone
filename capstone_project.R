#-------------------------------------------------------------------------
# This R script called 'capstone_project.R' was prepared for the milestone assignment 
# in the capstone project.
#
# Created By: Mark A. Jack
# Created On: 11/24/2016
#
#-------------------------------------------------------------------------
# Necessary library calls in R:
library(R.utils)
library(stringr)
library(knitr)
library(devtools)
library(quanteda)
library(slam)
#
library(tm)
library(NLP)
library(openNLP)
library(qdap)
library(RWeka)
library(ngram)
library(SnowballC)
#
library(ggplot2)
library(plyr)
library(dplyr)
#library(reshape2)
#
#-------------------------------------------------------------------------
# Set your work directory, garbage collection and clearing memory from prebious runs:
#setwd("~/")
#
# Garbage collection:
gc()
# To clean up the memory of your current R session run the following line:
rm(list=ls(all=TRUE))
gc()

# Create data files for further processing:
blogs <- readLines(file("./capstone-project/en_US.blogs.txt",encoding = "UTF-8"),encoding = "UTF-8", skipNul=TRUE)
news <- readLines(file("./capstone-project/en_US.news.txt",encoding = "UTF-8"),encoding = "UTF-8", skipNul=TRUE)
twitter <- readLines(file("./capstone-project/en_US.twitter.txt",encoding = "UTF-8"),encoding = "UTF-8", skipNul=TRUE)
close(file("./capstone-project/en_US.twitter.txt")) 
close(file("./capstone-project/en_US.news.txt")) 
close(file("./capstone-project/en_US.blogs.txt")) 
#
# Number of lines in each data file:
nl_blogs <- length(blogs)
nl_blogs
nl_news <- length(news)
nl_news
nl_twitter <- length(twitter)
nl_twitter
#
# Collapse text to a stream of characters to count number of characters in each data file 
# (remove " " defining individual words and lines of text):
nchar(paste(blogs, collapse = " "))
nchar(paste(news, collapse = " "))
nchar(paste(twitter, collapse = " "))
#
#-------------------------------------------------------------------------
# Create one corpus of text using the library 'quanteda',
# include beginning and of line descriptors,
# separate sentence fragments at intermediate punctuations:
#
require(quanteda)
sampleSize <- 0.01
set.seed(1234)
blogs.sample <- sample(blogs, nl_blogs*sampleSize)
news.sample <- sample(news, nl_news*sampleSize)
twitter.sample <- sample(twitter, nl_twitter*sampleSize)
doc.sample <- c(blogs.sample, news.sample, twitter.sample)
#
doc.sample <- gsub("^", " ", doc.sample)
doc.sample <- gsub("$", " ", doc.sample)
#doc.sample  <- gsub("[;,\\.\\_\\?\\!\\“\\”]", " ", doc.sample)
doc.corpus <- corpus(doc.sample)
#
# Check object sizes [in bytes] and print an example and a summary of the corpus of documents:
objectSize(blogs)
objectSize(blogs.sample)
objectSize(doc.sample)
objectSize(doc.corpus)
#
#head(doc.corpus, 5)
summary(doc.corpus, 5)
#
#-------------------------------------------------------------------------
# Transform text: Remove profanity, Create lowercase words, remove numbers, punctuations, stop words, stem of words, 
# and strip unnnecessary whitespace
#
# Download list of words of profanity from Website 
# http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
profanity <- readLines(file("./capstone-project/Terms-to-Block.csv",encoding = "UTF-8"),encoding = "UTF-8")
close(file("./capstone-project/Terms-to-Block.csv"))
#
# Tokenization: Create corpus of unigrams, bigrams, trigrams
#doc.tokens <- tokenize(toLower(doc.corpus), removePunct = TRUE, removeNumbers = TRUE, 
#              removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE)
#doc.nosw <- removeFeatures(doc.tokens, stopwords("english"))
#
unigrams.dfm  <- dfm(doc.corpus, ngrams = 1, ignoredFeatures = c(profanity), 
                     toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE, concatenator = " ",
                     removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = FALSE)
#
bigrams.dfm   <- dfm(doc.corpus, ngrams = 2, ignoredFeatures = c(profanity), 
                     toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE, concatenator = " ",
                     removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = FALSE)
#
trigrams.dfm  <- dfm(doc.corpus, ngrams = 3, ignoredFeatures = c(profanity), 
                     toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE, concatenator = " ",
                     removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = FALSE)
#
quadgrams.dfm <- dfm(doc.corpus, ngrams = 4, ignoredFeatures = c(profanity),
                     toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE, concatenator = " ",
                     removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = FALSE)
#
objectSize(unigrams.dfm)
objectSize(bigrams.dfm)
objectSize(trigrams.dfm)
objectSize(quadgrams.dfm)
#
saveRDS(unigrams.dfm, file="./capstone-project/unigrams_dfm.Rds")
saveRDS(bigrams.dfm, file="./capstone-project/bigrams_dfm.Rds")
saveRDS(trigrams.dfm, file="./capstone-project/trigrams_dfm.Rds")
saveRDS(quadgrams.dfm, file="./capstone-project/quadgrams_dfm.Rds")
#
#
write.csv(unigrams.dfm, file="./capstone-project/unigrams_dfm.csv")
write.csv(bigrams.dfm, file="./capstone-project/bigrams_dfm.csv")
write.csv(trigrams.dfm, file="./capstone-project/trigrams_dfm.csv")
write.csv(quadgrams.dfm, file="./capstone-project/quadgrams_dfm.csv")
#
#-------------------------------------------------------------------------
# Word frequencies
freq_uni  <- docfreq(unigrams.dfm)
freq_uni  <- freq_uni[order(freq_uni, decreasing = TRUE)]
freq_bi   <- docfreq(bigrams.dfm)
freq_bi   <- freq_bi[order(freq_bi, decreasing = TRUE)]
freq_tri  <- docfreq(trigrams.dfm)
freq_tri  <- freq_tri[order(freq_tri, decreasing = TRUE)]
freq_quad <- docfreq(quadgrams.dfm)
freq_quad <- freq_quad[order(freq_quad, decreasing = TRUE)]
#
# Create data frames:
unigrams.df <- data.frame(keyName=names(freq_uni), value = freq_uni, row.names=NULL)
bigrams.df  <- data.frame(keyName=names(freq_bi), value = freq_bi, row.names=NULL)
trigrams.df <- data.frame(keyName=names(freq_tri), value = freq_tri, row.names=NULL)
quadgrams.df <- data.frame(keyName=names(freq_quad), value = freq_quad, row.names=NULL)
#
objectSize(unigrams.df)
objectSize(bigrams.df)
objectSize(trigrams.df)
objectSize(quadgrams.df)

# ngram probabilities as obtained from the number of occurrences of different ngrams in the data set:
sum_unigrams <- sum(unigrams.df$value)
unigrams.df$prob <- unigrams.df$value / sum_unigrams
sum_bigrams <- sum(bigrams.df$value)
bigrams.df$prob <- bigrams.df$value / sum_bigrams
sum_trigrams <- sum(trigrams.df$value)
trigrams.df$prob <- trigrams.df$value / sum_trigrams
sum_quadgrams <- sum(quadgrams.df$value)
quadgrams.df$prob<- quadgrams.df$value / sum_quadgrams
#
head(unigrams.df, 10)
head(bigrams.df, 10)
head(trigrams.df, 10)
head(quadgrams.df, 10)
tail(unigrams.df, 10)
tail(bigrams.df, 10)
tail(trigrams.df, 10)
tail(quadgrams.df, 10)
#
# Number of different ngram types in the data set:
cnt_uni_types <- nrow(unigrams.df)
cnt_bi_types <- nrow(bigrams.df)
cnt_tri_types <- nrow(trigrams.df)
cnt_quad_types <- nrow(quadgrams.df)
cnt_uni_types
cnt_bi_types
cnt_tri_types
cnt_quad_types
#
# Perlexity measure for ngrams:
# Remove all ngrams (words / word combinations) that only occur once:
#unigrams.df <- unigrams.df[unigrams.df$value > 1, ]
#bigrams.df  <- bigrams.df[bigrams.df$value > 1, ]
#trigrams.df <- trigrams.df[trigrams.df$value > 1, ]
#quadgrams.df <- quadgrams.df[quadgrams.df$value > 1, ]
#
perplexity_uni <- exp(-sum(log(unigrams.df$prob)) / cnt_uni_types)
perplexity_bi <- exp(-sum(log(bigrams.df$prob)) / cnt_bi_types)
perplexity_tri <- exp(-sum(log(trigrams.df$prob)) / cnt_tri_types)
perplexity_quad <- exp(-sum(log(quadgrams.df$prob)) / cnt_quad_types)
perplexity_uni
perplexity_bi
perplexity_tri
perplexity_quad

# Kneser-Ney Smoothing:
# Kneser-Ney smoothing of the counts in bigrams.df, trigrams.df and quadgrams.df, 
# i.e. replace zero counts of bigrams, trigrams, quadgrams from the original training sample
# by non-zero estimates for the test data set and re-adjusting of the counts of observed 
# bigrams, trigrams, quadgrams in the training data set:

unisplit1 <- function(word) {
  strsplit(as.character(word), " ")[[1]][1]
}
unisplit2 <- function(word) {
  strsplit(as.character(word), " ")[[1]][2]
}
unisplit3 <- function(word) {
  strsplit(as.character(word), " ")[[1]][3]
}
unisplit4 <- function(word) {
  strsplit(as.character(word), " ")[[1]][4]
}
bigrams.df$uni1 <- vapply(bigrams.df$keyName, unisplit1, character(1))
bigrams.df$uni2 <- vapply(bigrams.df$keyName, unisplit2, character(1))

trigrams.df$uni1 <- vapply(trigrams.df$keyName, unisplit1, character(1))
trigrams.df$uni2 <- vapply(trigrams.df$keyName, unisplit2, character(1))
trigrams.df$uni3 <- vapply(trigrams.df$keyName, unisplit3, character(1))
trigrams.df$bi12 <- paste(trigrams.df$uni1, trigrams.df$uni2)
  
quadgrams.df$uni1 <- vapply(quadgrams.df$keyName, unisplit1, character(1))
quadgrams.df$uni2 <- vapply(quadgrams.df$keyName, unisplit2, character(1))
quadgrams.df$uni3 <- vapply(quadgrams.df$keyName, unisplit3, character(1))
quadgrams.df$uni4 <- vapply(quadgrams.df$keyName, unisplit4, character(1))
quadgrams.df$bi12 <- paste(quadgrams.df$uni1, quadgrams.df$uni2)
quadgrams.df$bi23 <- paste(quadgrams.df$uni2, quadgrams.df$uni3)
quadgrams.df$bi34 <- paste(quadgrams.df$uni3, quadgrams.df$uni4)
quadgrams.df$tri123 <- paste(quadgrams.df$uni1, quadgrams.df$uni2, quadgrams.df$uni3)
quadgrams.df$tri234 <- paste(quadgrams.df$uni2, quadgrams.df$uni3, quadgrams.df$uni4)

#head(unigrams.df, 10)
#head(bigrams.df, 10)
#head(trigrams.df, 10)
#head(quadgrams.df, 10)

# Set unigram continuation probabilities equal to probabilities from number of occurrences in 'unigrams.df' dataframe:
unigrams.df$cont_prob <- unigrams.df$prob

# Add continuation probabilities column to 'bigrams.df' dataframe:
D2 <- 0.75
f_uni_count1 <- function(word){ 
  unigrams.df$value[unigrams.df$keyName == word] 
  }
bi_uni_value1 <- vapply(bigrams.df$uni1, f_uni_count1, numeric(1))
#
f_uni_count2 <- function(word){ 
  length(which(bigrams.df$uni1 == word))
}
bi_uni_value2 <- vapply(bigrams.df$uni1, f_uni_count2, numeric(1))
#
f_uni_count3 <- function(word){ 
  length(which(bigrams.df$uni2 == word))
}
bi_uni_value3 <- vapply(bigrams.df$uni2, f_uni_count3, numeric(1))
#
bigrams.df$cont_prob <- abs(bigrams.df$value - D2) / bi_uni_value1 + D2 * bi_uni_value2 / bi_uni_value1 * bi_uni_value3 / cnt_bi_types


# Add continuation probabilities column to 'trigrams.df' dataframe:
D3 <- 0.75
f_bi_count1 <- function(word){ 
  bigrams.df$value[bigrams.df$keyName == word] 
  }
tri_bi_value1 <- vapply(trigrams.df$bi12, f_bi_count1, numeric(1))
#
f_bi_count2 <- function(word){ 
  length(which(trigrams.df$bi12 == word))
}
tri_bi_value2 <- vapply(trigrams.df$bi12, f_bi_count2, numeric(1))
#
f_bi_count3 <- function(word){ 
  length(which(trigrams.df$uni3 == word))
}
tri_bi_value3 <- vapply(trigrams.df$uni3, f_bi_count3, numeric(1))
#
trigrams.df$cont_prob <- abs(trigrams.df$value - D3) / tri_bi_value1 + D3 * tri_bi_value2 / tri_bi_value1 * tri_bi_value3 / cnt_tri_types


# Add continuation probabilities column to 'quadgrams.df' dataframe:
D4 <- 0.75
f_tri_count1 <- function(word){ 
  trigrams.df$value[trigrams.df$keyName == word] 
}
qd_tri_value1 <- vapply(quadgrams.df$tri123, f_tri_count1, numeric(1))
#
f_tri_count2 <- function(word){ 
  length(which(quadgrams.df$tri123 == word))
}
qd_tri_value2 <- vapply(quadgrams.df$tri123, f_tri_count2, numeric(1))
#
word3 <- quadgrams.df$keyName
f_tri_count3 <- function(word){ 
  length(which(quadgrams.df$uni4 == word))
}
qd_tri_value3 <- vapply(quadgrams.df$uni4, f_tri_count3, numeric(1))
#
quadgrams.df$cont_prob <- abs(quadgrams.df$value - D4) / qd_tri_value1 + D4 * qd_tri_value2 / qd_tri_value1 * qd_tri_value3 / cnt_quad_types

# Re-sort all ngrams dataframes starting with ngrams with highest continuation probabilities:
#attach(dplyr)
unigrams.df <- arrange(unigrams.df, desc(cont_prob))
bigrams.df <- arrange(bigrams.df, desc(cont_prob))
trigrams.df <- arrange(trigrams.df, desc(cont_prob))
quadgrams.df <- arrange(quadgrams.df, desc(cont_prob))
#
head(unigrams.df, 10)
head(bigrams.df, 10)
head(trigrams.df, 10)
head(quadgrams.df, 10)
#
saveRDS(unigrams.df, file="./capstone-project/unigram_model.Rds")
saveRDS(bigrams.df, file="./capstone-project/bigram_model.Rds")
saveRDS(trigrams.df, file="./capstone-project/trigram_model.Rds")
saveRDS(quadgrams.df, file="./capstone-project/quadgram_model.Rds")
#
write.csv2(unigrams.df, file="./capstone-project/unigram_model.csv", row.names=FALSE)
write.csv2(bigrams.df, file="./capstone-project/bigram_model.csv", row.names=FALSE)
write.csv2(trigrams.df, file="./capstone-project/trigram_model.csv", row.names=FALSE)
write.csv2(quadgrams.df, file="./capstone-project/quadgram_model.csv", row.names=FALSE)
#
#-------------------------------------------------------------------------
# Exploratory analysis of unigrams, bigrams and trigrams
#
# Create bar plots of 25 most frequent features in unigrams, bigrams, trigrams, quadgrams:
par(mar=c(4,4,2,2))
par(mfrow = c(1,1))
barplot(topfeatures(unigrams.dfm, 25), horiz=TRUE, las=1, col="Blue")
dev.copy(png, file = "./capstone-project/barplot_uni.png")
#dev.off()
#
par(mar=c(4,6,2,2))
par(mfrow = c(1,1))
barplot(topfeatures(bigrams.dfm, 25), horiz=TRUE, las=1, col="Green")
dev.copy(png, file = "./capstone-project/barplot_bi.png")
#dev.off()
#
par(mar=c(4,8,2,2))
par(mfrow = c(1,1))
barplot(topfeatures(trigrams.dfm, 25), horiz=TRUE, las=1, col="Red")
dev.copy(png, file = "./capstone-project/barplot_tri.png")
#dev.off()
#
par(mar=c(4,10,2,2))
par(mfrow = c(1,1))
barplot(topfeatures(quadgrams.dfm, 25), horiz=TRUE, las=1, col="Purple")
dev.copy(png, file = "./capstone-project/barplot_quad.png")
#dev.off()
#
head(unigrams.dfm)
head(bigrams.dfm)
head(trigrams.dfm)
head(quadgrams.dfm)
#-------------------------------------------------------------------------
unigrams.freq0 <- colSums(unigrams.dfm)
unigrams.freq <- sort(unigrams.freq0, decreasing=TRUE) 
#
bigrams.freq0 <- colSums(bigrams.dfm)
bigrams.freq <- sort(bigrams.freq0, decreasing=TRUE) 
#
trigrams.freq0 <- colSums(trigrams.dfm)
trigrams.freq <- sort(trigrams.freq0, decreasing=TRUE) 
#
quadgrams.freq0 <- colSums(quadgrams.dfm)
quadgrams.freq <- sort(quadgrams.freq0, decreasing=TRUE) 
#
#-------------------------------------------------------------------------
frequency <- 2
#
unigrams.most <- as.numeric()
for (i in 1:length(unigrams.freq)) { 
  if (unigrams.freq[i] > frequency) {
    unigrams.most  <- c(unigrams.most, unigrams.freq[i]) }
}
length(unigrams.most)
#
bigrams.most <- as.numeric()
for (i in 1:length(bigrams.freq)) { 
  if (bigrams.freq[i] > frequency) {
    bigrams.most  <- c(bigrams.most, bigrams.freq[i]) }
}
length(bigrams.most)
#
trigrams.most <- as.numeric()
for (i in 1:length(trigrams.freq)) { 
  if (trigrams.freq[i] > frequency) {
    trigrams.most  <- c(trigrams.most, trigrams.freq[i]) }
}
length(trigrams.most)
#
quadgrams.most <- as.numeric()
for (i in 1:length(quadgrams.freq)) { 
  if (quadgrams.freq[i] > frequency) {
    quadgrams.most  <- c(quadgrams.most, quadgrams.freq[i]) }
}
length(quadgrams.most)
#
# Create data frames, select 25 most frequent occurances in sorted lists of unigrams, bigrams and trigrams 
# and label columns:
unigrams_most <- data.frame(unigrams.most)
unigrams_most[, 1] <- as.character(names(unigrams.most))
unigrams_most[, 2] <- as.numeric(unigrams.most)
unigrams_most0 <- unigrams_most[1:25,]
colnames(unigrams_most0) <- c("Word","Frequency")
row.names(unigrams_most0) <- NULL
head(unigrams_most0)
#
bigrams_most <- data.frame(bigrams.most)
bigrams_most[, 1] <- as.character(names(bigrams.most))
bigrams_most[, 2] <- as.numeric(bigrams.most)
bigrams_most0 <- bigrams_most[1:25,]
colnames(bigrams_most0) <- c("Word","Frequency")
row.names(bigrams_most0) <- NULL
head(bigrams_most0)
#
trigrams_most <- data.frame(trigrams.most)
trigrams_most[, 1] <- as.character(names(trigrams.most))
trigrams_most[, 2] <- as.numeric(trigrams.most)
trigrams_most0 <- trigrams_most[1:25,]
colnames(trigrams_most0) <- c("Word","Frequency")
row.names(trigrams_most0) <- NULL
head(trigrams_most0)
#
quadgrams_most <- data.frame(quadgrams.most)
quadgrams_most[, 1] <- as.character(names(quadgrams.most))
quadgrams_most[, 2] <- as.numeric(quadgrams.most)
quadgrams_most0 <- quadgrams_most[1:25,]
colnames(quadgrams_most0) <- c("Word","Frequency")
row.names(quadgrams_most0) <- NULL
head(quadgrams_most0)
#
#-------------------------------------------------------------------------
# Alternative:
#unigrams.df <- readRDS(file="/Users/markjack/Desktop/unigram_model.Rds")
#bigrams.df <- readRDS(file="/Users/markjack/Desktop/bigram_model.Rds")
#trigrams.df <- readRDS(file="/Users/markjack/Desktop/trigram_model.Rds")
#quadgrams.df <- readRDS(file="/Users/markjack/Desktop/quadgram_model.Rds")
#
#unigrams_most <- data.frame(unigrams.df[, 1:2])
#unigrams_most[, 1] <- as.character(unigrams.df$keyName)
#unigrams_most[, 2] <- as.numeric(unigrams.df$value)
#colnames(unigrams_most) <- c("Word","Frequency")
#unigrams_most <- arrange(unigrams_most, desc(Frequency))
#unigrams_most0 <- unigrams_most[1:25,]
#row.names(unigrams_most0) <- NULL
#head(unigrams_most0)
#
#bigrams_most <- data.frame(bigrams.df[, 1:2])
#bigrams_most[, 1] <- as.character(bigrams.df$keyName)
#bigrams_most[, 2] <- as.numeric(bigrams.df$value)
#colnames(bigrams_most) <- c("Word","Frequency")
#bigrams_most <- arrange(bigrams_most, desc(Frequency))
#bigrams_most0 <- bigrams_most[1:25,]
#row.names(bigrams_most0) <- NULL
#head(bigrams_most0)
#
#trigrams_most <- data.frame(trigrams.df[, 1:2])
#trigrams_most[, 1] <- as.character(trigrams.df$keyName)
#trigrams_most[, 2] <- as.numeric(trigrams.df$value)
#colnames(trigrams_most) <- c("Word","Frequency")
#trigrams_most <- arrange(trigrams_most, desc(Frequency))
#trigrams_most0 <- trigrams_most[1:25,]
#row.names(trigrams_most0) <- NULL
#head(trigrams_most0)
#
#quadgrams_most <- data.frame(quadgrams.df[, 1:2])
#quadgrams_most[, 1] <- as.character(quadgrams.df$keyName)
#quadgrams_most[, 2] <- as.numeric(quadgrams.df$value)
#colnames(quadgrams_most) <- c("Word","Frequency")
#quadgrams_most <- arrange(quadgrams_most, desc(Frequency))
#quadgrams_most0 <- quadgrams_most[1:25,]
#row.names(quadgrams_most0) <- NULL
#head(quadgrams_most0)
#
#-------------------------------------------------------------------------
# Plots of histograms of unigram, bigram, trigram, quadgram occurrances:
g1 <- ggplot(data = unigrams_most0, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Blue") 
g2 <- g1 + geom_text(aes(label=Frequency), vjust=-0.20) + labs(title = "Frequency of unigrams") 
g3 <- g2 + labs(x = "Unigrams", y = "Frequency")
g4 <- g3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g4
dev.copy(png, file = "./capstone-project/unigrams_freq.png")
#dev.off()

g1 <- ggplot(data = bigrams_most0, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Green") 
g2 <- g1 + geom_text(aes(label=Frequency), vjust=-0.20) + labs(title = "Frequency of bigrams") 
g3 <- g2 + labs(x = "Bigrams", y = "Frequency")
g4 <- g3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g4
dev.copy(png, file = "./capstone-project/bigrams_freq.png")
#dev.off()

g1 <- ggplot(data = trigrams_most0, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Red") 
g2 <- g1 + geom_text(aes(label=Frequency), vjust=-0.20) + labs(title = "Frequency of trigrams") 
g3 <- g2 + labs(x = "Trigrams", y = "Frequency")
g4 <- g3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g4
dev.copy(png, file = "./capstone-project/trigrams_freq.png")
#dev.off()

g1 <- ggplot(data = quadgrams_most0, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Purple") 
g2 <- g1 + geom_text(aes(label=Frequency), vjust=-0.20) + labs(title = "Frequency of quadgrams") 
g3 <- g2 + labs(x = "Quadgrams", y = "Frequency")
g4 <- g3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g4
dev.copy(png, file = "./capstone-project/quadgrams_freq.png")
#dev.off()

# Create a word cloud plot:
if (require(RColorBrewer))
 plot(unigrams.dfm, max.words = 200, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
 dev.copy(png, file = "./capstone-project/unigrams_cloud.png")
 #dev.off()
 
 plot(bigrams.dfm, max.words = 200, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
 dev.copy(png, file = "./capstone-project/bigrams_cloud.png")
 #dev.off()
 
 plot(trigrams.dfm, max.words = 200, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
 dev.copy(png, file = "./capstone-project/trigrams_cloud.png")
 #dev.off()
 
 plot(quadgrams.dfm, max.words = 200, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
 dev.copy(png, file = "./capstone-project/quadgrams_cloud.png")
 #dev.off()
