#
# server.R
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

source("global.R")

# Create bar plots of 25 most frequent features in unigrams, bigrams, trigrams, quadgrams:

# ngram plots
shinyServer(function(input, output) {

  #======================================================================
  # Word prediction:
  dataInput <- reactive({tolower(input$text)})
  output$prediction <- renderPrint({ paste(dataInput(), guess_next_word(dataInput())) })
  #output$prediction <- renderPrint({ paste(tolower(input$text), guess_next_word(tolower(input$text))) })

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

  #======================================================================
  # Plot:
  dataInputP <- reactive({
    input$select
  })
  
  output$distPlot <- renderPlot({
    par(mfrow = c(1,1))
    par(mar=c(4,6,2,2))
    
    data <- switch(dataInputP(), 
                   "unigrams" = unigrams_most0,
                   "bigrams" = bigrams_most0,
                   "trigrams" = trigrams_most0,
                   "quadgrams" = quadgrams_most0)
    
    color <- switch(dataInputP(), 
                    "unigrams" = "blue",
                    "bigrams" = "red",
                    "trigrams" = "darkorange",
                    "quadgrams" = "darkviolet")
    
    title <- switch(dataInputP(), 
                    "unigrams" = "Frequency of unigrams",
                    "bigrams" = "Frequency of bigrams",
                    "trigrams" = "Frequency of trigrams",
                    "quadgrams" = "Frequency of quadgrams")
    
    g1 <- ggplot(data = data, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill=color) 
    g2 <- g1 + geom_text(aes(label=Frequency), vjust=-0.20) + labs(title = title) 
    g3 <- g2 + labs(x = dataInputP(), y = "Frequency")
    g4 <- g3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    g4
    
  })
})
