library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shiny)
library(tidyverse)
library(maps)

happydata <- read_csv("2016.csv") 

datatable(happydata)

tbl <- table(happydata$Score, happydata$Region)
tbl


ggplot(happydata, aes(x = Region)) + geom_bar() + xlab("By Region")

dfavg <- happydata %>%
  select(Region, `Score`) %>%
  group_by(Region) %>%
  summarize(Average = mean(`Score`)) %>%
  arrange(desc(Average))
dfavg

#boxplot
hover <- ggplot(data = happydata, aes(x = happydata$Region, y = happydata$`Score`)) +
  geom_boxplot(aes(color = Region, fill = Region), alpha = 0.5) +
  geom_point(aes(color = Region), position = position_jitter(width = .1)) +
  labs(title = "Happiness by World Region", 
       x = "Region", 
       y = "Happiness Score") +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(2.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text.x = element_blank()) 
fig <- ggplotly(hover)

#Correlation
names(happydata)[names(happydata)=="Economy"] <- "Economy"
names(happydata)[names(happydata)=="Health"] <- "Health"
names(happydata)[names(happydata)=="Corruption"] <- "Trust"
pairs(~ Economy+Family+Health+Freedom+Trust+Generosity, data = happydata, 
      main="Importances of the Six Factors of Happiness")

#top ten
topten <- happydata %>% head(10)

dflong <- gather(topten, Factor, `Importance of Factor`, Economy:Generosity, factor_key=TRUE)

ggplot(data = dflong) +
  geom_bar(stat = "identity", 
           aes(x = Country, y = `Importance of Factor`, fill = Factor)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "The Six Factors of Happiness in the Ten Happiest Countries") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))

#worldmap
worldmap <- map_data("world")
names(worldmap)[names(worldmap)=="region"] <- "Country"
worldmap$Country[worldmap$Country == "USA"] <- "United States"
happy_world <- happydata %>%
  full_join(worldmap, by = "Country")

map_theme <- theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_rect(fill = "white"))

ggplot(data = happy_world, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = `Score`))  +
  scale_fill_continuous(low="yellow", high="purple", na.value="snow2") +
  coord_quickmap() +
  labs(title = "Happiness Around the World - 2015") +
  map_theme
