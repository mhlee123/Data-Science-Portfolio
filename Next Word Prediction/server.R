library(shiny)
library(ggplot2)
library(ggthemes)
library(caret)
library(rpart)
require(reshape2)
library(gtools)
library(dplyr)

unigrams <-  readRDS("unigrams.RDS", refhook = NULL)
bigrams <-  readRDS("bigrams.RDS", refhook = NULL)
trigrams <-  readRDS("trigrams.RDS", refhook = NULL)
quadgrams <-  readRDS("quadgrams.RDS", refhook = NULL)

shinyServer(function(input, output) {
  
  ngram_model <- function(prefix, ngrams,n_minus_1_gram){
    prefix <- trimws(prefix, "r")
    
    obs_n_minus_1_gram <- n_minus_1_gram %>% filter(ngram == prefix) 
    
    prefix <- paste0(prefix," ")

    
    obs_ngrams <- ngrams %>% filter(startsWith(ngram, prefix)) %>% 
      mutate(prob = freq/obs_n_minus_1_gram$freq) %>%
      arrange(desc(freq)) %>% head(3)
    if(nrow(obs_ngrams) == 0){
      last_words <- character()
    }else{
      last_words <- apply(obs_ngrams,1,function(x) tail(strsplit(x[1]," ")[[1]],1))
    }
    return(last_words)
  }
  
  
  predict_next_Word <- function(input){
    predicted <- vector(mode = "character")
    input_length <- sapply(strsplit(input, " "), length)
    
    if(input_length == 1){
      predicted <- ngram_model(input, bigrams,unigrams)
      if(length(predicted) < 3){
        prefix <- ""
        predicted <- append(predicted,predict_next_Word(prefix))
      }
    }else if(input_length == 2){
      predicted <- ngram_model(input, trigrams,bigrams)
      if(length(predicted) < 3){
        prefix <- word(input,input_length)
        predicted <- append(predicted,predict_next_Word(prefix))
      }
    }else if(input_length >= 3){
      prefix <- paste(word(input,(input_length-2):input_length), collapse = ' ')
      predicted <-  ngram_model(prefix, quadgrams,trigrams)
      
      if(length(predicted) < 3){
        prefix  <- paste(word(input,(input_length-1):input_length), collapse = ' ')
        predicted <- append(predicted,predict_next_Word(prefix))
      }
    }else{
      predicted <- top_3_unigrams
    }
    
    return(head(predicted,3))
  }
  
  value <- eventReactive(input$predict, {
    
    # predict outcome
    input <- tolower(input$userinput)
    predicted <- predict_next_Word(input)
    predicted
    
  })

  output$predict_1 <- renderText({
    paste0("\"",value()[1],"\"")
  })
  
  output$predict_2 <- renderText({
    paste0("\"",value()[2],"\"")
  })
  
  output$predict_3 <- renderText({
    paste0("\"",value()[3],"\"")
  })
    
  output$text1 <- renderText({ 
    x <-"Are these the words you are looking for?"
    x
  })
    
  
  
  
  
})
