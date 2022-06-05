library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

ui <- basicPage(
  plotOutput('plot1', click = 'plot_click'),
  checkboxInput("income_level", "Personal Income", FALSE),
  checkboxInput("age", "Age", FALSE)
)

server <- function(input, output) {
  
  current_query <- c()
  df <- renderData()
  
  output$plot1 <- getPlot(df, current_query)
  
  observeEvent(input$income_level, {
    if (input$income_level == TRUE) {
      current_query <- addToQuery(current_query, "Personal.Income")
      df <- renderData(current_query)
      output$plot1 <- getPlot(df, current_query)
    } else {
      current_query <- deleteFromQuery(current_query, "Personal.Income")
      df <- renderData(current_query)
      output$plot1 <- getPlot(df, current_query)
    }
  }
    
  )
}

shinyApp(ui, server)