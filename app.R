library(shiny)
library(ggplot2)

ui <- basicPage(
  plotOutput('plot1', click = 'plot_click'),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  df <- read.csv('ca_edu_data.csv')
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(Year, Population.Count, color=Personal.Income)) + geom_point()
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)