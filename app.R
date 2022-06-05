library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

ui <- basicPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "color",
        "Color:",
        c("None"="NULL",  "Personal Income"="Personal.Income", "Age"="Age", "Gender"="Gender",
          "Educational.Attainment"="Educational.Attainment"),
        selected = "None"
      ),
      selectInput(
        "group",
        "Group:",
        c("None"="NULL",  "Personal Income"="Personal.Income", "Age"="Age", "Gender"="Gender",
          "Educational.Attainment"="Educational.Attainment"),
        selected = "None"
      )
    ),
  mainPanel(
    plotOutput('plot1', click = 'plot_click')
  )
)
)

server <- function(input, output) {
  
  current_query <- c()
  df <- renderData()
  
  output$plot1 <- getPlot(df, current_query)
  
  observeEvent(input$color, {
    print(input$color)
    if (input$color == 'NULL') {
      current_query <- c()
    } else {
      current_query <- c(input$color)
    }
    print("current query is...")
    print(current_query)
    df <- renderData(current_query)
    output$plot1 <- getPlot(df, current_query)
    }
  )
  
}

shinyApp(ui, server)