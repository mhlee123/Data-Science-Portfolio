#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)

shinyUI(
  fluidPage(
    tags$head(tags$style(
      type="text/css",
      "img1 {height:450px;width:80%;align:center}"
    )),
    theme = shinytheme("flatly"),
    
    navbarPage("Text Prediction App",
               tabPanel("Prediction",
                        fluidPage(
                          sidebarLayout(
                            
                            sidebarPanel(
                              textInput("userinput", "Enter text and press enter", ""),
                              actionButton("predict","Enter",width = '100%')
                            ),
                            mainPanel(
                              h4(textOutput("text1")),
                              fluidRow(
                                column(width=4, h3(tags$i(textOutput("predict_1")))),
                                column(width=4, h3(tags$i(textOutput("predict_2")))),
                                column(width=4, h3(tags$i(textOutput("predict_3"))))
            
                            )
                          )
                          
                        )
                        )
               ),
               
               tabPanel("Documentation",
                        fluidPage(
                          strong(h3('About')),
                          p('The goal of the capstone project is to build a data product that is able to predict the next three most probable words given input text using the basic knowlege of NLP.'),
                          p('The datasets used are extracted from blogs, news and twitter feeds with over millions of rows'),
                          p('The predictive model makes use of N-grams table covering 1-gram, 2-grams, 3-grams and 4-grams'),
                          p(),
                          p('Dataset is downloadable at: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip.'),
                          strong(h3('Algorithm')),
                          tags$ul(
                            tags$li("The algorithm assigns probabilies to ngams using Maximum Likelihood Estimate (MLE)."), 
                            tags$li("The algorithm  employs a 'stupid Back off' approach."), 
                            tags$li("This method always takes the prediction from the highest-order ngrams. "), 
                            tags$li("If no matching grams are found, it will back off to N-1 grams."),
                            tags$li("For example, the algorithm will first try to find the word that match the last 3 words in the quadgrams table, if less than 3 words are found, the algorithm will look for the words that match the last 2 words in the trigrams table.")
                          ),
                          
                        strong(h3('How to use')),
                        tags$ol(
                          tags$li("Type/Insert word(s) into the text box provided in the left panel."), 
                          tags$li("Press 'Enter' button."), 
                          tags$li("You will be presented with three predicted next words."), 
                          tags$li("Enjoy!")
                        )
                        )
               )
    )
    )
  
)