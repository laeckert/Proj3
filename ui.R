#load in packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyverse)
library(devtools)
library(DT)
library(shinyWidgets)

#read in dataset

happydata <- as_tibble(read_csv("2016.csv"))

#Create the basis for our dashboard
dashboardPage(skin = "purple",
              dashboardHeader(title = "World Happiness", titleWidth = "250"),
              dashboardSidebar(
                  sidebarMenu(
                      menuItem("Information", tabName = "Information", icon = icon("cat")),
                      menuItem("Data Page", tabName = "Data", icon=icon('chart-bar')),
                      menuItem("Data Exploration", tabName = "Exploration", icon=icon("arrow-down")),
                      menuItem("PCA", tabName = "PCAnalysis", icon=icon('connectdevelop')),
                      menuItem("Modeling Page", tabName = "Models", icon=icon('feather-alt'))
                  )
              ),
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "Information", strong(h2("Information About this Application and Data", style="color:navy")),
                              box(background = "olive", width=12, 
                                  h5("The is the final project for Lucy Eckert in ST558. I built an application to look at how measurements of well-being affect happiness. Do countries with the greatest wealth have the most happiness? Do the healthiest countries have the most happiness? With this app you can review how different factors will predict happiness.")),
                              box(background = "light-blue", width=12, h5("My data comes from the World Happiness Report, which is a publication of the Sustainable Development Solutions Network. The report was first published in 2012, and I looked at the 2016 dataset.")), 
                              box(background = "navy", width=12, h5("Documentation"), uiOutput("tab"), uiOutput("datatab"))),
                      
                      
                      tabItem(tabName = "Data", strong(h2("Data Tables")),
                              
                              fluidRow(box(title="Expand for More Info-->", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="orange",
                                           h5("This page allows you to view and download the data by region, and to select multiple regions at once. You can use the carrots to sort the data.")),
                                       pickerInput("region","Regions", choices=levels(as.factor(happydata$Region)), options = list(`actions-box` = TRUE),multiple = T,selected = "Western Europe"),
                                      
                                       downloadButton("DownloadData", "Download", "Data"),
                                       hr(), 
                                       dataTableOutput("Tables"))),
                      
                      tabItem(tabName = "PCAnalysis", strong(h2("Principal Component Analysis")),
                              fluidRow(
                                  box(background="blue", width=10, collapsible = TRUE, collapsed=TRUE, 
                                      title="Expand for More Info-->", h5("Principal Component Analysis, also known as PCA, is useful in large datasets when there isnt a response variable. Although my dataset is fairly small, and I have a response variable, I have built this analysis and biplot to see how the set up and analysis work.")),
                                  
                                  selectizeInput("PCs", "Choose the principal components", choices = names(happydata[c(5,6,7,8,9,10)])),
                                  
                                  selectizeInput("biReg", "Look to see biplot that considers the above selected variable", choices = levels(as.factor(happydata$Region))),
                                  uiOutput("biplottext"),
                                  br(), 
                                  box(title = "BiPlot", width = 10, plotOutput("Biplot")))),
                      
                      tabItem(tabName = "Models", strong(h2("Data Modeling")), 
                              
                              box(title="Expand for More Info-->", h4("I have included below two types of supervised learning models: a simple linear regression model and a multiple linear regression model.For the simple linear regression model, we have one predictor value that helps us estimate a response. In this case, the response is the Score, or what I have been calling the Happiness value. To help us determine whether the predictor is useful, we look at the R-squared value, which is a measure of correlation. For multiple linear regression (MLR) uses several explanatory, or predictor, variables. The R-square in MLR shows the relationship between predictors and their impact on the response. I have included examples of each of the types of supervised learning below."), 
                                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=10, background = "navy", 
                                  withMathJax(), helpText(h4('An example of a simple linear regression formula - 
                                           $$y = b_1x_1 + a$$ or Y = a + bX')), 
                                  helpText(h4('An example of a multiple linear reg. model - 
                                $$y = b_1x_1 + b_2x_2 + b_3x_1x_2 + e$$'))),
                              
                              selectizeInput("slrmodel", "Simple Linear Reg. Model - choose the explanatory variable to visualize the relationship to the happiness value/Score.", names(happydata)[c(5,7,8)]),
                              uiOutput("modeltext"),
                              br(),
                              plotOutput("plot1"),
                              
                              sliderInput("Health", "Predict Happiness Value by Health Rating", min=0, max=1, value=0.3), 
                              
                              tableOutput("mtable"),
                              
                              radioButtons("mlrpreds", "Choose the Multiple Linear Regression Variables", 
                                           choices=c("Effect of Freedom and Family on Happiness", 
                                                     "Effect of Generosity and Economy on Happiness", 
                                                     "Effect of Health and Family on Happiness")),
                              
                              tableOutput("mlrmodel")),
                      

                      
                      tabItem(tabName = "Exploration", h2(uiOutput("title")),
                              column(width=8, 
                                     box(title="Expand for More Info-->", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=50, background = "green", 
                                         h5("Use the dropdowns to select and X and Y variable and explore the correlation between different measurements of well-being. Although you can select from a number of options for the Y axis, the best result will be had from selecting score for the Y axis. Try clicking on the plot to get the x and y at a given location. You can also change the size of the dot with the slider below. Once the plot is to your liking, you can download a png of the plot. I hope you enjoy exploring this data."))),
                              
                              fluidRow(
                                  selectizeInput("regs", "Regions", choices = levels(as.factor(happydata$Region))),
                                  selectInput("x", "Please Select a Variable for the X-axis", choices = c("Economy","Family", "Health", "Freedom", 
                                                                    "Corruption", "Generosity","Score"),selected = "Health"),
                                  selectInput("y", "Please Select a Variable for the Y-axis", choices = c("Economy","Family", "Health", "Freedom", 
                                                                    "Corruption", "Generosity","Score"),selected = "Score"),
                                  
                                  plotOutput("dataplot", click = "plot_click", width = "500", height = "350px"),
                                  br(), 
                                  downloadButton("DownloadPlot", "Download Plot"),
                                  hr(), 
                                  sliderInput("size", "Size of Points on Graph",
                                              min = 1, max = 10, value = 3, step = 1),
                                  
                                  checkboxInput("check", "Reset All Entries"),
                                  
                                  verbatimTextOutput("info"),
                                  
                                  box(h2("Numeric Summary of Health Score"), textOutput("text")),
                                  
                                  box(dataTableOutput("table"), title="Click to see table of plot data", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))
                      )
                  )
              )
)


