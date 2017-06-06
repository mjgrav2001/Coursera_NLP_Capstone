#
# myfile.R
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


# Create bar plots of 25 most frequent features in unigrams, bigrams, trigrams, quadgrams:

#' @post / guess_next_word
guess_next_word <- function(typed_sentence) { 
    
    typed_sentence <- as.character(typed_sentence)
    typed_sentence  <- gsub("[;,\\.\\_\\?\\!\\“\\”]", " ", typed_sentence)
    
    cnt_uni <- nrow(unigrams.df)
    cnt_bi <- nrow(bigrams.df)
    cnt_tri <- nrow(trigrams.df)
    cnt_quad <- nrow(quadgrams.df)
    
    if (nchar(typed_sentence) == 0) { 
      best_guess <- c("Suggestion: Do you want to start with typing e.g. 'I, you, he, she, the, this, that, and, or, of, in, to'?")      
      return (best_guess)
    }
    else {

    words <- strsplit(as.character(typed_sentence), " ")[[1]] 
    
    lw <- length(words)
    lw1 <- lw-1
    lw2 <- lw-2
    
    if (lw >= 3) {
      trigram_typed <- paste0(strsplit(as.character(typed_sentence), " ")[[1]][lw2:lw], collapse = " ") 
      bigram_typed  <- paste0(strsplit(as.character(typed_sentence), " ")[[1]][lw1:lw], collapse = " ") 
      unigram_typed <- paste0(strsplit(as.character(typed_sentence), " ")[[1]][lw], collapse = " ") 
      
      word_guess <- head(quadgrams.df$uni4[quadgrams.df$tri123 == trigram_typed], 3)
      
      if (length(word_guess) >= 1) {
        best_guess <- word_guess
        return (best_guess)
      }
      else {
        word_guess <- head(trigrams.df$uni3[trigrams.df$bi12 == bigram_typed], 3)
        
        if (length(word_guess) >= 1) {
          best_guess <- word_guess
          return (best_guess)
        }
        else {
          word_guess <- head(bigrams.df$uni2[bigrams.df$uni1 == unigram_typed], 3) 
          
          if (length(word_guess) >= 1) {
            best_guess <- word_guess
            return (best_guess)
          }
          else {
            best_guess <- c("ERROR: No matching words on record. Please correct or type and submit another word.")
            return (best_guess)
          }
        }
      }
    }
    else if (lw == 2) {
      bigram_typed  <- paste0(strsplit(as.character(typed_sentence), " ")[[1]][1:2], collapse = " ") 
      unigram_typed <- paste0(strsplit(as.character(typed_sentence), " ")[[1]][2], collapse = " ") 
      
      word_guess <- head(trigrams.df$uni3[trigrams.df$bi12 == bigram_typed], 3)
      if (length(word_guess) >= 1) {
        best_guess <- word_guess
        return (best_guess)
      }
      else {
        word_guess <- head(bigrams.df$uni2[bigrams.df$uni1 == unigram_typed], 3)
        
        if (length(word_guess) >= 1) {
          best_guess <- word_guess
          return (best_guess)
        }
        else {
          best_guess <- c("ERROR: No matching words on record. Please correct or type and submit another word.")
          return (best_guess)
        }
      }
    }
    else {
      unigram_typed <- strsplit(as.character(typed_sentence), " ")[[1]][1]  
      word_guess <- head(bigrams.df$uni2[bigrams.df$uni1 == unigram_typed], 3)
      
      if (length(word_guess) >= 1) {
        best_guess <- word_guess
        return (best_guess)
      }
      else {
        best_guess <- c("ERROR: No matching words on record. Please correct or type and submit another word.")
        return (best_guess)
      }
    }
    
    }
    return (best_guess)
}    

