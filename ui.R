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
                      menuItem("Information - Start Here!", tabName = "Information", icon = icon("cat")),
                      menuItem("Data Page", tabName = "Data", icon=icon('chart-bar')),
                      menuItem("Data Exploration", tabName = "Exploration", icon=icon("arrow-down")),
                      menuItem("PCA", tabName = "PCAnalysis", icon=icon('connectdevelop')),
                      menuItem("Modeling Page", tabName = "Models", icon=icon('feather-alt'))
                  )
              ),
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "Information", em(h2("Information About this Application and Data", style="color:navy")),
                              box(background = "olive", width=12, 
                                  h5("The is the final project for Lucy Eckert in ST558. I built an application to look at how measurements of well-being affect happiness. Do countries with the greatest wealth have the most happiness? Do the healthiest countries have the most happiness? With this app you can review how different factors will predict happiness.")),
                              box(background = "light-blue", width=12, h5("My data comes from the World Happiness Report, which is a publication of the Sustainable Development Solutions Network. The report was first published in 2012, and I looked at the 2016 dataset.")), 
                              box(background = "navy", width=12, h5("Documentation"), uiOutput("tab"), uiOutput("datatab"))),
                      
                      
                      tabItem(tabName = "Data", strong(em(h2("Data Tables"))),
                              
                              fluidRow(box(title="Expand for More Info-->", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="navy",
                                           h5("This page allows you to view and download the data by region")),
                                       pickerInput("region","Regions", choices=levels(as.factor(happydata$Region)), options = list(`actions-box` = TRUE),multiple = T,selected = "Western Europe"),
                                      
                                       downloadButton("DownloadData", "Download", "Data"),
                                       hr(), 
                                       dataTableOutput("Tables"))),
                      
                      tabItem(tabName = "PCAnalysis", strong(em(h2("Principal Component Analysis"))),
                              fluidRow(
                                  box(background="maroon", width=10, collapsible = TRUE, collapsed=TRUE, 
                                      title="About PCA", h4("Principal Component Analysis, also known as PCA, is...")),
                                  
                                  selectizeInput("PCs", "Choose the principal components", choices = names(happydata[c(5,6,7,8,9,10)])),
                                  
                                  selectizeInput("biReg", "Look to see biplot that considers the above selected variables", choices = levels(as.factor(happydata$Region))),
                                  uiOutput("biplottext"),
                                  br(), 
                                  box(title = "BiPlot", width = 10, plotOutput("Biplot")))),
                      
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
                              
                              selectizeInput("slrmodel", "Simple Linear Reg. Model - choose the explanatory variable to visualize relation to the death rate", names(happydata)[c(4,7)]),
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
                      

                      
                      tabItem(tabName = "Exploration", h2(uiOutput("title")),
                              column(width=8, 
                                     box(title="About", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width=50, background = "navy", 
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
                                  
                                  box(h2("Numeric Summary of Data"), textOutput("text")),
                                  
                                  box(dataTableOutput("table"), title="Click to see table of plot data", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))
                      )
                  )
              )
)


