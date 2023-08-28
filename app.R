library(shiny)
library(dplyr)
library(ggplot2)
source("helpers.R")

# Render UI
ui <- basicPage(
  tags$script(src = "https://kit.fontawesome.com/7b969bf8cd.js"),
  titlePanel("Californian Population Income Data Widget"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "x",
        "X Variable:",
        c("Year"="Year", "Personal Income"="Personal.Income", "Age"="Age",
          "Gender"="Gender", "Educational Attainment"="Educational.Attainment"),
        selected = "Year"
      ),
      selectInput(
        "color",
        "Color Variable:",
        c("None"="NULL",  "Personal Income"="Personal.Income", "Age"="Age",
          "Gender"="Gender", "Educational Attainment"="Educational.Attainment"),
        selected = "None"
      ),
      div(style = "padding-top: 5px; padding-bottom: 5px; font-size: 11px",
         '*Population figures averaged over all years when "Year" is not selected as variable.'),
      a(tags$i(class="fa-solid fa-house", style = "font-size:3rem;"),
        href = "http://sarahegood.com/"),
      a(tags$i(class="fa-brands fa-github", style = "font-size:3rem;"),
        href = "https://github.com/SarahEGood/ca_shiny_app"),
      a(tags$i(class="fa-brands fa-linkedin", style = "font-size:3rem;"),
        href = "https://www.linkedin.com/in/segood/"),
      a(tags$i(class="fa-brands fa-mastodon", style = "font-size:3rem;"),
        href = "https://tech.lgbt/@sarahegood"),
      div(class = 'footer',
          style = "padding-top: 5px; font-size: 11px",
          includeHTML("footer.html"))
    ),
  mainPanel(
    plotOutput('plot1', click = 'plot_click'),
    plotOutput('plot2'),
    dataTableOutput('table')
    )
  )
)

server <- function(input, output) {
  # Init initial data
  current_query <- c()
  df <- renderData("Year", "Population.Count")
  # Draw initial plot
  output$plot1 <- getPlot(df, "Year", "Population.Count", current_query)
  output$plot2 <- getPropPlot(df, "Year", "Population.Count", current_query)
  # observeEvents to change plot upon changing params
  # If color variable is changed, apply to plot and table
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
  # If x variable is changed, apply to plot and table
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