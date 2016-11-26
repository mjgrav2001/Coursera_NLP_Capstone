#
# global.R
#
# Purpose:    This application creates a shiny app for a natural language processing model 
#             using Kneser-Ney smoothing with calculated continuation probabilities 
#             for the prediction of 'ngrams'
# Created By: Mark A. Jack
# Created On: 11/24/2016
#
library(shiny)
library(plyr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)
#
unigrams.df <- readRDS(file="unigram_model.Rds")
bigrams.df <- readRDS(file="bigram_model.Rds")
trigrams.df <- readRDS(file="trigram_model.Rds")
quadgrams.df <- readRDS(file="quadgram_model.Rds")
#
unigrams_most <- data.frame(unigrams.df[, 1:2])
unigrams_most[, 1] <- as.character(unigrams.df$keyName)
unigrams_most[, 2] <- as.numeric(unigrams.df$value)
colnames(unigrams_most) <- c("Word","Frequency")
unigrams_most <- arrange(unigrams_most, desc(Frequency))
unigrams_most0 <- unigrams_most[1:25,]
row.names(unigrams_most0) <- NULL
#
bigrams_most <- data.frame(bigrams.df[, 1:2])
bigrams_most[, 1] <- as.character(bigrams.df$keyName)
bigrams_most[, 2] <- as.numeric(bigrams.df$value)
colnames(bigrams_most) <- c("Word","Frequency")
bigrams_most <- arrange(bigrams_most, desc(Frequency))
bigrams_most0 <- bigrams_most[1:25,]
row.names(bigrams_most0) <- NULL
#
trigrams_most <- data.frame(trigrams.df[, 1:2])
trigrams_most[, 1] <- as.character(trigrams.df$keyName)
trigrams_most[, 2] <- as.numeric(trigrams.df$value)
colnames(trigrams_most) <- c("Word","Frequency")
trigrams_most <- arrange(trigrams_most, desc(Frequency))
trigrams_most0 <- trigrams_most[1:25,]
row.names(trigrams_most0) <- NULL
#
quadgrams_most <- data.frame(quadgrams.df[, 1:2])
quadgrams_most[, 1] <- as.character(quadgrams.df$keyName)
quadgrams_most[, 2] <- as.numeric(quadgrams.df$value)
colnames(quadgrams_most) <- c("Word","Frequency")
quadgrams_most <- arrange(quadgrams_most, desc(Frequency))
quadgrams_most0 <- quadgrams_most[1:25,]
row.names(quadgrams_most0) <- NULL
