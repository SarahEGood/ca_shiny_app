library(shiny)
library(ggplot2)

ui <- basicPage(
  plotOutput('plot1', click = 'plot_click'),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  df <- read.csv('ca_edu_data.csv')
  df$Year <- as.POSIXct(df$Year, format= '%m/%d/%Y %H:%M:%S %p')
  df$Year <- format(df$Year, format='%Y')
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(Year, Population.Count, color=Personal.Income)) + geom_point()
  }, res = 96)
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)