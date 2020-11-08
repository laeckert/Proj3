
library(shiny)
library(markdown)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(markdown)
library(maps)


#Load in our data 
happydata <- read_csv("2016.csv")
regional <- happydata %>% group_by(Region)

#Set up URLs 
url <- a("My github page", href="https://github.com/laeckert/Proj3")
urldata <- a("Dataset", href="https://www.kaggle.com/unsdsn/world-happiness?select=2016.csv")

shinyServer(function(input, output, session) {
  
  output$datatab <- renderUI({
    tagList("Here you can find the dataset:", urldata)
  })
  output$tab <- renderUI({
    tagList("Here you can see visit my github page to see code:", url)
  })
  
  #downloadable csv of dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(region, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(region, file, row.names = FALSE)
    }
  )
  
  #plot for exploretab
  output$boxplot <- renderPlot({
    ggplot(data = happydata, aes(x = happydata$Region, y = happydata$`Score`)) +
      geom_boxplot(aes(color = Region, fill = Region), alpha = 0.5) +
      geom_point(aes(color = Region), position = position_jitter(width = .1)) +
      labs(title = "Happiness by World Region", 
           x = "Region", 
           y = "Happiness Score") +
      theme_minimal() +
      theme(plot.title = element_text(size = rel(2.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_blank())
  })
  
  #create text info
  output$info <- renderText({
    #get filtered data
    
    paste("The average happiness score for", input$choice, "is", round(mean(happydata$Score, na.rm = TRUE), 2))
  })
  
  #table for access tab
  output$happytable <- DT::renderDataTable({
    happydata
  })




})
