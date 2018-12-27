#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(caret)
library(rpart)
require(reshape2)
library(gtools)


data1 <- read.csv('insurance.csv',header = TRUE)

shinyServer(function(input, output) {
  
  show <- FALSE
  
  output$`insurance-img` <- renderImage({
      return(list(
        src = "www/insurance.jpg",
        contentType = "image/jpeg",
        width = 700,
        height = 450
      ))
      
    }, deleteFile = FALSE)
  

  
  output$data <- DT::renderDataTable( 
    DT::datatable(
      data1
    )
  )
  
  output$scatterplot <- renderPlot({
      p <- ggplot(data1,aes_string(x=input$x1, y=data1$charges))+geom_point() + ylab("Charges") + ggtitle(paste('Scatter Plot of insurance charges versus',input$x1)) 
      p + theme_stata() + scale_colour_stata(scheme = "s2color")
  
  })
  
  output$boxplot <- renderPlot({
    p <- ggplot(data1, aes_string(x=input$x2, y=data1$charges, fill = input$x2)) + geom_boxplot() + ylab("Charges")+ ggtitle(paste('Box Plot of insurance charges versus',input$x2)) 
    p + theme_stata() + scale_colour_stata(scheme = "s1color")
    
  })
  p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
    geom_boxplot()

 
  model <- lm(charges ~ ., data = data1)
  
  value <- eventReactive(input$predict, {
    
    newrow <- list("age" = as.integer(input$age), "sex" = input$sex, "bmi" = input$bmi,"children" = as.numeric(input$child),"smoker" = input$smoker,"region" = input$region,"charges" = 1.11)
    
    # predict outcome
    predicted <- predict(model, newrow)
    
    predicted
  })
  
  output$predicted <- renderText({
    value()
  })

  observeEvent(input$predict, {
    
    output$`dollar-sign` <- renderImage({
      return(list(
        src = "www/dollar-sign.png",
        contentType = "image/png",
        width = 50,
        height = 50
      ))
      
      
    }, deleteFile = FALSE)
 
    output$text1 <- renderText({ 
      x <-"Predicted Insurance Cost:"
      x
    })
    
    
    
  })
  
  
  
})
