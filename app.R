library(shiny)
library(dplyr)
library(ggplot2)

ui <- basicPage(
  plotOutput('plot1', click = 'plot_click'),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  df <- read.csv('ca_edu_data.csv')
  df$Year <- as.POSIXct(df$Year, format= '%m/%d/%Y %H:%M:%S %p')
  df$Year <- format(df$Year, format='%Y')
  
  df[is.na(df)] <- 0
  
  df <- df %>% group_by(Year) %>% summarise(
    Population.Count = sum(Population.Count, na.rm=FALSE)
  )
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(x=Year, y=Population.Count, group=1)) + geom_line() + geom_point()
  }, res = 96)
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)