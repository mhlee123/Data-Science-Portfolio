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
data1 <- read.csv('insurance.csv',header = TRUE)
shinyUI(
  fluidPage(
    tags$head(tags$style(
      type="text/css",
      "img1 {height:450px;width:80%;align:center}"
    )),
    theme = shinytheme("flatly"),
    
    navbarPage("Insurance Cost Prediction",
       tabPanel("Prediction",
          fluidPage(
            sidebarLayout(
              
              sidebarPanel(
                h3('Predictive Factors'),
                sliderInput("age", "Age:",
                            min = min(data1$age), max =  max(data1$age), value = min(data1$age)
                ),
                selectInput("sex", "Sex:",unique(data1$sex)),
                
                sliderInput("bmi", "BMI:",
                            min = min(data1$bmi), max =  max(data1$bmi), value = min(data1$bmi)
                ),
                selectInput("child", "Children:",unique(data1$children)),
                selectInput("smoker", "Smoker:",unique(data1$smoker)),
                selectInput("region", "Region:",unique(data1$region)),
                
                actionButton("predict","Predict",width = '100%')
              ),
              mainPanel(
                titlePanel('Choose the individual factors and find out what will be their insurance cost'),
                imageOutput('insurance-img', width = "100%", height = "100%", inline = FALSE),
                  fluidRow(
                    column(width=3, h4(textOutput("text1"))),
                    column(width=1,imageOutput('dollar-sign', width = "100%", height = "100%", inline = FALSE)),
                    column(width=2,style='margin-top:-10px', strong(h3(textOutput("predicted"))))
                  )
                
                
              )
            )
   
      )
      ),
       tabPanel("Visualization",
          fluidPage(
            titlePanel("Relationship Plot"),
            sidebarLayout(
                
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.tabs == 'Scatter Plot'",
                    selectInput("x1", "Independent variables:",c("age","bmi"))
                   ),
                  conditionalPanel(
                    condition = "input.tabs == 'Box Plot'",
                    selectInput("x2", "Independent variables:",c("sex","children","smoker","region"))
                  ),
                  strong(p('Target variable:')),
                  p('Charges')
              ),
            mainPanel(
              tabsetPanel(id="tabs",
                          tabPanel("Scatter Plot", 
                                   plotOutput("scatterplot")),
                          tabPanel("Box Plot", 
                                   plotOutput("boxplot"))
              )
              
            )
            )
            
           
          )
       ),
       tabPanel("Dataset",
          # Create a row for the table.
          fluidPage(
            fluidRow(
              DT::dataTableOutput("data")
            )
          )
       ),
       tabPanel("Documentation",
        fluidPage(
          strong(h3('About the Dataset')),
          p('The dataset contains the demographics information of 1338 individuals and their medical costs. Below are the columns of the dataset:'),
          tags$ul(
            tags$li("age: age of primary beneficiary"), 
            tags$li("sex: insurance contractor gender, female, male"), 
            tags$li("bmi: Body mass index, providing an understanding of body, weights that are relatively high or low relative to height, objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9"), 
            tags$li("children: Number of children covered by health insurance / Number of dependents"),
            tags$li("smoker: Smoking"),
            tags$li("region: the beneficiary's residential area in the US, northeast, southeast, southwest, northwest."),
            tags$li("charges: Individual medical costs billed by health insurance")
          ),
          p('Dataset available at: https://www.kaggle.com/mirichoi0218/insurance'),
          strong(h3('About the Project')),
          p('This Shiny application aims to perform exploratory analysis on the dataset and predict the insurance cost of a particular individual using
            multiple regression. The target variable is charges and the rest of the variables are used as the predictors.')
        )        
        )
    )
  )
)