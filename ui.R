
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shiny)
library(tidyverse)
library(maps)



shinyUI(
    dashboardPage(skin = "yellow",
        dashboardHeader(title = "What Brings Happiness? A Data Exploration", 
                        titleWidth = "520"),
        dashboardSidebar(
            menuItem("Information",
                     tabName = "info",
                     icon = icon("cat")),
br(),
            menuItem("Data Exploration",
                     tabName = "explore",
                     icon = icon("chart-bar")),
br(),
            menuItem("Clustering",
                     tabName = "cluster",
                     icon = icon("connectdevelop")),
br(),            
            menuItem("Modeling",
                     tabName = "modeling",
                     icon = icon("feather-alt")),
br(),                     
            menuItem("Access Data",
                     tabName = "access",
                     icon = icon("arrow-down"))
        ),
        dashboardBody(
            tabItems(
                
                tabItem(tabName = "info",
                        em(h3("Describing the App and Data", style="color:navy")),
                        box(background = "purple", width=50,
                            h5("This dataset blah blah")),
                        box(background = "green", width=50,
                            h5("Final Project for Lucy Eckert in ST558. Add some sappy shit
                               about happiness here.")),
                        box(background = "fuchsia", width=50,
                            h5("Supporting Info"), uiOutput("tab"), uiOutput("datatab")),
                        box(background = "teal", width=50,
                            h5("Here is how you navigate this app..."))),
                
                tabItem(tabName = "explore", width=50,
                        
                        fluidRow(box(plotOutput("boxplot"))),
                        box(selectInput("choice", "Select Region to Summarize ", choices = levels(as.factor(happydata$Region)))),
                        box(h5(textOutput("info")))),
                
                tabItem(tabName = "cluster"),
                
                tabItem(tabName = "modeling"),
                
                tabItem(tabName = "access", strong(em("Select and Download Data by Region")),
                    selectizeInput("region", "Region", choices = levels(as.factor(happydata$Region))),
                    downloadButton("downloadData", "Download"),
                    hr(),
                    dataTableOutput("happytable"))
                )
            )
        )
           
    )
    

