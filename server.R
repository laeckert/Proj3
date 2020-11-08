
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
  
  
  
  output$happytable = DT::renderDataTable({
    happydata
  })




})
