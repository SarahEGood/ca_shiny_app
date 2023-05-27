library(dplyr)
library(shiny)
library(forcats)

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

convertToVar <- function(x, y, df) {
  return(df()[df()[,input$first] == input$second,])
}

renderData <- function(x, y, query=NULL) {
  df <- read.csv('ca_edu_data.csv')
  df$Year <- as.POSIXct(df$Year, format= '%m/%d/%Y %H:%M:%S %p')
  df$Year <- format(df$Year, format='%Y')
  
  df[is.na(df)] <- 0
  
  if (!is.null(query)) {
    std_query <- c(x, y, query)
  } else {
    std_query <- c(x, y)
  }
  
  print('std_query is...')
  print(std_query)
  
  df <- df %>% group_by_at(std_query)
  df[, names(df) == x] <- sapply(df[, names(df) == x], as.factor)
  
  df <- calcProportions(df, x, y, query)
  return(df)
}

query <- ""

calcProportions <-function(df, x, y, query) {
  mod_query <- c(x, y, query)
  mod_query <- unique(mod_query[mod_query != ""])
  if (length(mod_query) == 3) {
    main <- df[, mod_query] %>% group_by_at(c(x)) %>% 
      summarise_at(y, sum)
    names(main)[names(main) == y] <- 'Totals'
    main <- main[, (names(main) %in% c(x, query, 'Totals'))]
    tmp <- df[, mod_query] %>% group_by_at(c(x, query)) %>% summarise_each(sum)
    joined <- merge(x=tmp, y=main, by=x, all.x=TRUE)
    joined$per <- joined$Population.Count / joined$Totals * 100
  } else {
    main <- df[, mod_query] %>% group_by_at(x) %>% 
      summarise_at(y, sum)
    joined <- main
    joined$per <- 1
  }
  
  if ('Educational.Attainment' %in% mod_query) {
    joined <- joined %>%
      mutate(Educational.Attainment = fct_relevel(Educational.Attainment,
        'Children under 15', 'No high school diploma',
        'High school or equivalent', 'Some college, less than 4-yr degree',
        "Bachelor's degree or higher"))
  }
  
  if ('Personal.Income' %in% mod_query) {
    joined <- joined %>%
      mutate(Personal.Income = fct_relevel(Personal.Income,
        'No Income', '$5,000 to $9,999', '$10,000 to $14,999',
        '$15,000 to $24,999', '$25,000 to $34,999', '$35,000 to $49,999',
        '$50,000 to $74,999', '$75,000 and over'))
  }
  
  return(joined)
}

getPlot <- function(df, x, y, current_query) {
  
  if (is.null(current_query)) {
  target_graph <- renderPlot({
    ggplot(df, aes_string(x, y, group=1)) +
      geom_line() +
      geom_point()
  }, res = 96)
  } else {
    query <- parseQuery(current_query)
    print('query is...')
    print(query)
    target_graph <- renderPlot({
      ggplot(df, aes_string(x=x, y=y, group=query)) +
        geom_line(aes_string(color=query)) +
        geom_point(aes_string(color=query)) +
        ylab('Population')
    }, res = 96)
  }
  
  return (target_graph)
}

getPropPlot <- function(df, x, y, current_query) {

  if (is.null(current_query)) {
    target_graph <- renderPlot({
      ggplot(df, aes_string(x, "per", group=1)) +
        geom_line() +
        geom_point() +
        ylab('Proportion of Population')
    }, res = 96)
  } else {
    query <- parseQuery(current_query)
    print('query is...')
    print(query)
    if (x == 'Educational.Attainment') {
      target_graph <- renderPlot({
        ggplot(df, aes_string(x=x, y="per", group=query)) +
          geom_line(aes_string(color=query)) +
          geom_point(aes_string(color=query)) +
          ylab('Proportion of Population') +
          scale_x_discrete(limits = c("Children under 15",
            "No high school diploma", "High school or equivalent",
            "Some college, less than 4-yr degree",
            "Bachelor's degree or higher"))
      }, res = 96)
    } else if (x == 'Personal.Income'){
      target_graph <- renderPlot({
        ggplot(df, aes_string(x=x, y="per", group=query)) +
          geom_line(aes_string(color=query)) +
          geom_point(aes_string(color=query)) +
          ylab('Proportion of Population') +
          scale_x_discrete(limits = c('No Income', '$5,000 to $9,999', '$10,000 to $14,999',
                                      '$15,000 to $24,999', '$25,000 to $34,999', '$35,000 to $49,999',
                                      '$50,000 to $74,999', '$75,000 and over'))
        }, res = 96)
    } else {
      target_graph <- renderPlot({
        ggplot(df, aes_string(x=x, y="per", group=query)) +
          geom_line(aes_string(color=query)) +
          geom_point(aes_string(color=query)) +
          ylab('Proportion of Population')
      }, res = 96)
    }
  }
  
  return (target_graph)
}