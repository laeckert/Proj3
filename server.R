#load in packages
library(shiny)
library(shinydashboard)
library(readr)
library(devtools)
library(dplyr)
library(DT)
library(ggplot2)
library(caret)
library(ggfortify)

#Load in our data and filter by year

happydata <- as_tibble(read_csv("2016.csv"))

#Set up URLs 
url <- a("Githib repo", href="https://github.com/laeckert/Proj3")
urldata <- a("World Happiness Report", href="https://www.kaggle.com/unsdsn/world-happiness")

shinyServer(function(input, output, session){
  
  #Link URLs to the dataset and personal github page. 
  output$datatab <- renderUI({
    tagList("Background info on dataset:", urldata)
  })
  
  output$tab <- renderUI({
    tagList("Visit my github page to see code:", url)
    
  })
  
  #Get our Filtered Data dynamically. 
  getData <- reactive({
    newData <- happydata %>% filter(happydata$Region == input$regs)
  })
  getData1 <- reactive({
    newData <- happydata %>% filter(happydata$Region == input$region)
  })
  getData2 <- reactive({
    newData <- happydata %>% filter(happydata$Region == input$biReg)
  })
  
  #UI to reset to default settings
  observe({
    if(input$check){
      updateSliderInput(session, "size", min=1, max=10, value=1)
      updateTextInput(session, "info")
    }
    else
      updateSliderInput(session, "size", min=1, max=10, value=5)
  })
  
  #create plot
  output$dataplot <- renderPlot({
    
    #get filtered data
    newData <- getData()
    #create plot
    g <- ggplot(happydata, aes_string(x=input$x, y=input$y)) 
    g + geom_point(size=input$size, aes(col=input$regs)) + theme(legend.position = "none")  
  })
  
  #Make a reactive plot. 
  plotInput <- reactive({
    #get filtered data
    newData <- getData()
    
    #create plot
    g <- ggplot(newData, aes_string(x=input$x, y=input$y)) 
    g + geom_point(size=input$size, aes(col=input$regs))
  })
  
  #download plot to png 
  output$DownloadPlot <- downloadHandler(
    filename = function() { paste(input$regs, '.png', sep='') },
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    })
  
  #dynamic title
  output$title <- renderUI({
    text <- HTML("Exploring Data <br> <br> Scatterplot of Regions:",
                 input$regs)
  })
  
  #to click on plot
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  output$biplottext <- renderUI({
    if(input$PCs=="Economy"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Economy variables for ", input$biReg,". ")
    }
    else if(input$PCs=="Family"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Family variables for ", input$biReg,". ")
    }
    
    else if(input$PCs=="Health"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Health variables for ", input$biReg,". ")
    }
    
    else if(input$PCs=="Freedom"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Freedom variables for ", input$biReg,". ")
    }
    
    else if(input$PCs=="Corruption"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Corruption variables for ", input$biReg,". ")
    }
    
    else if(input$PCs=="Generosity"){
      paste0("Here we look at the linear combination of the Happiness Rank 
                   and the Generosity variables for ", input$biReg,". ")
    }
    
  })
  
  #Create text for model output
  output$modeltext <- renderUI({
    if(input$slrmodel=="Rank"){
      paste0("It looks like that as we have the Year to measure our Happiness, we visualize an interesting 
                   regression plot. We can view the frequency or # of occurrences with how the levels of Happiness
                   that we have. For example, for 2015, we have Happiness relatively spread out, with the higher levels
                   having many occurrences 150 and 250 for the Happiness. As for the previous years, we see that they are 
                   more concentrated around a smaller interval such as 150 and 200. This may seem to indicate that as year
                   grows, the death rate may be variable and highly unpredictable given the top 10 causes of death in the U.S.")
    }
    
    else if(input$slrmodel=="Health"){
      paste0("It looks like the number of Health appears to be a great predictor of the death rate, as expected. 
                   Since the Death rate resembles the number of Health from a population given a certain time frame, the 
                   Health that we have seem to be correlated with our death rate. ")
    }
  })
  
  #Get our numeric summaries
  output$text <- renderText({
    #get filtered data
    newData <- getData()
    paste("The average Health score for", input$regs, 
          "is", round(mean(newData$Health, na.rm = TRUE), 2), 
          "and the average happiness score is", 
          round(mean(newData$Score, na.rm = TRUE), 2), sep = " ")
  })
  
  #create table of output observations for data table page
  output$Tables <- renderDataTable({
    newdata <- getData1()
  })
  
  #create table of output observations for exploration page
  output$table <- renderDataTable({
    newdata <- getData() 
    print(newdata)
  })
  
  #button to download a csv file of the data
  output$DownloadData <- downloadHandler(
    filename = function(){
      paste(input$region, ".csv", sep="")
    },
    content = function(file){
      write.csv(getData(), file, row.names = FALSE)
    }
  )
  
  #Begin making dynamic user interface for choosing models 
  ModelData <- reactive({
    happydata[, c(input$slrmodel, "Score")]
  })
  
  #1st supervised learning model
  #Create simple models to choose from
  model1 <- lm(Score ~ Health, data=happydata)
  model2 <- lm(Score ~ Rank, data=happydata)
  model3 <- lm(Score ~ Country, data=happydata)
  sum_mod1 <- summary(model1)
  
  output$plot1 <- renderPlot({
    par(mar=c(5.1, 4.1, 0, 1))
    plot(ModelData())
    if(input$slrmodel=='Health'){
      abline(a=sum_mod1$coefficients[1,1], b=sum_mod1$coefficients[2,1])
    }
    if(input$slrmodel=='Rank'){abline(model2)}
    if(input$slrmodel=='Country'){abline(model3)}
  })
  #Create Biplots
  output$Biplot <- renderPlot({
    
    newdata <- getData2()
    dapcs <- prcomp(select(newdata, Score, Health))
    fapcs <- prcomp(select(newdata, Score, Family))
    capcs <- prcomp(select(newdata, Score, Corruption))
    eapcs <- prcomp(select(newdata, Score, Economy))
    rapcs <- prcomp(select(newdata, Score, Freedom))
    gapcs <- prcomp(select(newdata, Score, Generosity))
    
    if(input$PCs=="Health"){
      biplot(dapcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Green")
    }
    
    else if(input$PCs=="Family"){
      biplot(fapcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Blue")
    }
    
    else if(input$PCs=="Corruption"){
      biplot(capcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Green")
    }
    
    else if(input$PCs=="Economy"){
      biplot(eapcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Purple")
    }
    
    else if(input$PCs=="Freedom"){
      biplot(rapcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Purple")
    }
    
    else if(input$PCs=="Generosity"){
      biplot(gapcs, xlabs=rep(".", nrow(newdata)), cex=1.3, col="Purple")
    }
    
  })
  
  #Predict our death rate based on # of Health. Used a sliderinput. 
  output$mtable <- renderTable({
    modeldata <- data.frame(Health = happydata$Health, Score = happydata$Score)
    model_lm <- lm(Score ~ Health, data=modeldata)
    ddata <- data.frame(Health=input$Health)
    preddata <- predict(model_lm, ddata)
    
  })
  
  #Make the models for our MLR models. 
  model4 <- lm(Score ~ Health + Region, data=happydata)
  model5 <- lm(Score ~ Health + Rank, data=happydata)
  model6 <- lm(Score ~ Region + Rank, data=happydata)
  
  #Summaries to get our coefficients of R-Squared. 
  sum_mod4 <- summary(model4)
  sum_mod5 <- summary(model5)
  sum_mod6 <- summary(model6)
  
  #Second supervised learning model - MLR
  output$mlrmodel<- renderTable({
    if(input$mlrpreds=="Effect of Health and Cause on Happiness"){
      paste("R-squared: ", sum_mod4$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
            "As the R-squared is high, we can conclude that the # of Health and Cause are highly related in measuring the death rate.")
    }
    else if(input$mlrpreds=="Effect of Health and Year on Happiness"){
      paste("R-squared: ", sum_mod5$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
            "As the R-squared is low, we can conclude that the # of Health and Year aren't very related in measuring the death rate.")
    }
    else if(input$mlrpreds=="Effect of Cause and Year on Happiness"){
      paste("R-squared: ", sum_mod6$r.squared, "As this is a multiple linear reg. model, we interpret the R-squared value as how the cause and year predictors are related.",
            "As the R-squared is high, we can conclude that the cause and year are highly related in measuring the death rate.")
      
    }
  })
  
})

