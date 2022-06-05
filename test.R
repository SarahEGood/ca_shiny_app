library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

df <- renderData()
current_query = c('Personal.Income')

df <- renderData('Personal.Income')
getPlot(df, current_query)

ggplot(df, aes_string(x="Year", y="Population.Count", group=current_query)) +
  geom_line(aes_string(color=query)) +
  geom_point(aes_string(color=query))