library(dplyr)
library(shiny)

parseQuery <- function(query) {
  return(paste(query, collapse=", "))
}

addToQuery <- function(query, new_item) {
  if (!(new_item %in% query)) {
    return (append(query, new_item))
  } else {
    return(query)
  }
}

deleteFromQuery <- function(query, to_delete) {
  if (to_delete %in% query) {
    return(query[! query %in% to_delete])
  } else {
    return(query)
  }
}

renderData <- function(query=NULL) {
  df <- read.csv('ca_edu_data.csv')
  df$Year <- as.POSIXct(df$Year, format= '%m/%d/%Y %H:%M:%S %p')
  df$Year <- format(df$Year, format='%Y')
  
  df[is.na(df)] <- 0
  
  if (!is.null(query)) {
    std_query <- addToQuery(c('Year'), query)
  } else {
    std_query <- 'Year'
  }
  df <- df %>% group_by_at(std_query) %>% summarise(
    Population.Count = sum(Population.Count, na.rm=FALSE)
  )
  print('std_query is...')
  print(std_query)
  return(df)
}

getPlot <- function(df, current_query) {
  
  if (is.null(current_query)) {
  target_graph <- renderPlot({
    ggplot(df, aes_string("Year", "Population.Count", group=1)) +
      geom_line() +
      geom_point()
  }, res = 96)
  } else {
    query <- parseQuery(current_query)
    print('query is...')
    print(query)
    target_graph <- renderPlot({
      ggplot(df, aes_string(x="Year", y="Population.Count", group=query)) +
        geom_line(aes_string(color=query)) +
        geom_point(aes_string(color=query))
    }, res = 96)
  }
  
  return (target_graph)
}