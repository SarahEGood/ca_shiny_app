library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

ui <- basicPage(
  titlePanel("Californian Population Income Data Widget"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "x",
        "X:",
        c("Year"="Year", "Personal Income"="Personal.Income", "Age"="Age", "Gender"="Gender",
          "Educational Attainment"="Educational.Attainment"),
        selected = "Year"
      ),
      selectInput(
        "color",
        "Color:",
        c("None"="NULL",  "Personal Income"="Personal.Income", "Age"="Age", "Gender"="Gender",
          "Educational Attainment"="Educational.Attainment"),
        selected = "None"
      ),
      fluidRow(
        a(img(src="github-mark.png", align='center', height='25px',width='25px'),
          href = "https://github.com/SarahEGood/ca_shiny_app"),
        a(img(src="linkedin.png", align='center', height='25px',width='25px'),
          href = "https://www.linkedin.com/in/segood/")
      )
    ),
  mainPanel(
    plotOutput('plot1', click = 'plot_click'),
    plotOutput('plot2'),
    dataTableOutput('table')
  )
)
)

server <- function(input, output) {
  
  current_query <- c()
  df <- renderData("Year", "Population.Count")
  
  output$plot1 <- getPlot(df, "Year", "Population.Count", current_query)
  output$plot2 <- getPropPlot(df, "Year", "Population.Count", current_query)
  
  observeEvent(input$color, {
    print(input$color)
    if (input$color == 'NULL') {
      current_query <- c()
    } else {
      current_query <- c(input$color)
    }
    print("current query is...")
    print(current_query)
    df <- renderData(input$x, "Population.Count", current_query)
    output$plot1 <- getPlot(df, input$x, "Population.Count", current_query)
    output$plot2 <- getPropPlot(df, input$x, "Population.Count", current_query)
    output$table <- renderDataTable(df)
    }
  )
  
  observeEvent(input$x, {
    if (input$color == 'NULL') {
      current_query <- c()
    } else {
      current_query <- c(input$color)
    }
    df <- renderData(input$x, "Population.Count", current_query)
    output$plot1 <- getPlot(df, input$x, "Population.Count", current_query)
    output$plot2 <- getPropPlot(df, input$x, "Population.Count", current_query)
    output$table <- renderDataTable(df)
  })
  
}

shinyApp(ui, server)