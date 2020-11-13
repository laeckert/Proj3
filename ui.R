#load in packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyverse)
library(devtools)
library(DT)

#read in dataset
data <- as_tibble(read_csv("2016.csv"))
pdata <- as_tibble(read_csv("2016.csv"))

#Create the basis for our dashboard
dashboardPage(skin = "green",
              dashboardHeader(title = "World Happiness", titleWidth = "250"),
              dashboardSidebar(
                  sidebarMenu(
                      menuItem("Information - Start Here!", tabName = "Information", icon = icon("cat")),
                      menuItem("Data Page", tabName = "Data", icon=icon('chart-bar')),
                      menuItem("Data Exploration", tabName = "Exploration", icon=icon("arrow-down")),
                      menuItem("Unsupervised Learning", tabName = "PCAnalysis", icon=icon('connectdevelop')),
                      menuItem("Modeling Page", tabName = "Models", icon=icon('feather-alt'))
                  )
              ),
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "Information", em(h2("Describing the App and Data", style="color:navy")),
                              box(background = "purple", width=12, 
                                  h4("Final Project for Lucy Eckert in ST558")),
                              box(background = "teal", width=12, h5("Final Project for Lucy Eckert in ST558")), 
                              box(background = "yellow", width=12, h5("Links to Documentation!"), uiOutput("tab"), uiOutput("datatab"))), 
                      
                      tabItem(tabName = "PCAnalysis", strong(em(h2("Unsupervised Learning"))),
                              fluidRow(
                                  box(background="green", width=10, collapsible = TRUE, collapsed=TRUE, 
                                      title="About PCA", h4("Principal Component Analysis, also known as PCA, is...")),
                                  
                                  selectizeInput("PCs", "Choose the principal components", choices = names(pdata[c(4,5,6,7,8,9,10)])),
                                  
                                  selectizeInput("cauze", "Look to see biplot based on causes", choices = levels(as.factor(pdata$Region))),
                                  uiOutput("biplottext"),
                                  br(), 
                                  box(title = "BiPlot", width = 6, plotOutput("Biplot")))),
                      
                      tabItem(tabName = "Models", strong(em(h2("Data Modeling"))), 
                              
                              box(title="About", h4("We will plot 2 types of supervised models: a simple linear regression model and a multiple linear regression model. 
                                      For a simple linear regression model, note that we have one predictor value that helps us estimate a response. 
                                      To help us determine whether the predictor is useful, we can generally look at the R-squared value, which is a measure of correlation. 
                                      For multiple linear regression, we have more than one predictor value, or we can have a combination of multiple values as well. The R-square here 
                                      represents the relationship between our predictors and their impact on the response. Below you can see examples of the models written in math form."), 
                                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", 
                                  withMathJax(), helpText(h4('An example of a simple linear reg. model - 
                                           $$y = b_1x_1 + e$$')), 
                                  helpText(h4('An example of a multiple linear reg. model - 
                                $$y = b_1x_1 + b_2x_2 + b_3x_1x_2 + e$$'))),
                              
                              selectizeInput("slrmodel", "Simple Linear Reg. Model - choose the explanatory variable to visualize relation to the death rate", names(pdata)[c(4,7)]),
                              uiOutput("modeltext"),
                              br(),
                              plotOutput("plot1"),
                              
                              sliderInput("Health", "Predict Death Rate by Number of Deaths", min=1000, max=50000, value=10000), 
                              
                              tableOutput("mtable"),
                              
                              radioButtons("mlrpreds", "Choose the mlr predictors", 
                                           choices=c("Effect of Meow and Meow on Happiness", 
                                                     "Effect of Meow and Meow on Happiness", 
                                                     "Effect of Meow and Meow on Happiness")),
                              
                              tableOutput("mlrmodel")),
                      
                      tabItem(tabName = "Data", strong(em(h2("Data Tables"))),
                              
                              fluidRow(box(title="Expand for More Info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="green",
                                           h5("In this app, we explore the factors that were analysed in a study that examined what makes populations happy. This data was ")),
                                       
                                       selectizeInput("cause", "Causes", choices = levels(as.factor(pdata$Region))),
                                       downloadButton("DownloadData", "Download", "Data"),
                                       hr(), 
                                       dataTableOutput("Tables"))),
                      
                      tabItem(tabName = "Exploration", h2(uiOutput("title")),
                              column(width=5, 
                                     box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "green", 
                                         h5("In this app, we are looking at filtered data for all 50 States across from 2010 to 2015. By selecting the 
                       from the causes of death in the dropdown menu, we can view how the number of deaths have impacted the death rate in the five year interval. We can visualize
                       this in a plot, we can visualize the average of the deaths compared to the death rate, and we can also see a subset of our data of interest in a table."))),
                              
                              fluidRow(
                                  selectizeInput("causes", "Causes", choices = levels(as.factor(pdata$Region))),
                                  
                                  plotOutput("dataplot", click = "plot_click", width = "500", height = "350px"),
                                  br(), 
                                  downloadButton("DownloadPlot", "Download Plot"),
                                  hr(), 
                                  sliderInput("size", "Size of Points on Graph",
                                              min = 1, max = 10, value = 3, step = 1),
                                  
                                  checkboxInput("check", "Reset All Entries"),
                                  
                                  verbatimTextOutput("info"),
                                  
                                  box(h2("Numeric Summary of Data"), textOutput("text")),
                                  
                                  box(dataTableOutput("table"), title="Click to see table of plot data", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))
                      )
                  )
              )
)


