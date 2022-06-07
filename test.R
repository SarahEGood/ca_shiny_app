library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

df <- renderData()
current_query = c('Personal.Income')

df <- renderData(c('Personal.Income','Population.Count','Educational.Attainment'))
getPlot(df, current_query)

df1 <- df
df1$Group <- paste(df1$Age, df1$Educational.Attainment)

ggplot(df1, aes_string(x="Year", y="Population.Count", color="Group")) +
  geom_line(aes_string(group="Group")) +
  geom_point(aes_string(group="Group"))

query <- c('Age')