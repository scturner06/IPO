# Author: Sam Turner
# Script for doing data exploration on IPO data
# Will make maps, etc...

# LIBRARIES
library(shiny)
library(plotly)
library(zipcode)
library()

# FUNCTIONS
readData <- function(dir, ext, classes = NA) {
  # Reads in files and puts them in a list of dataframes
  #
  # Args:
  #   dir: Directory where files are located
  #   classes: Vector of classes to be used in read.csv colClasses arg
  #
  # Returns:
  #   A list of dataframes containing the files in dir with the .csv extension
  #   removed.
  setwd(dir)
  files <- list.files(pattern = paste0("*.", ext))
  
  if(ext == "csv")
    data <- lapply(files, read.csv, colClasses = classes)
  if(ext == "xlsx") {
    data <- lapply(files, read.xlsx2
                   , sheetIndex = 1
                   , colClasses = classes
                   , stringsAsFactors = FALSE)
  }
  names(data) <- gsub("\\..*", "", files)
  
  return(data)
}

server <- function(input, output, session) {
  data.ipo.count <- read.csv("data/IPO_count.csv"
                             , stringsAsFactors = FALSE)
  
  data.ipo.count <- transform(data.ipo.count
                              , IPO.Count = as.numeric(IPO.Count)
                              , lat = as.numeric(lat)
                              , lon = as.numeric(lon))
  
  data.marketCap <- read.csv("data/MarketCap.csv"
                             , stringsAsFactors = FALSE)
  data.marketCap$ZIP <- clean.zipcodes(data.marketCap$ZIP)
  data.marketCap$Year <- lapply(data.marketCap$Year, function(x) paste0(x, ".01"))
  colnames(data.marketCap)[1] = "Time"
  data.marketCap <- transform(data.marketCap
                              , Time = as.character(Time)
                              , Total.MarketCap = as.numeric(Total.MarketCap)
                              , Cum.MarketCap = as.numeric(Cum.MarketCap))
  
  data("zipcode")
  
  time.sqft <- read.csv("data/sqft/Time.csv", colClasses = "character")
  colnames(time.sqft) <- "Time"
  
  time.value <- read.csv("data/value/Time.csv", colClasses = "character")
  colnames(time.value) <- "Time"
  
  output$plot <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    
    plot_geo(data.ipo.count, locationmode = 'USA-states'
             , sizes = c(1, 250), source = "map") %>%
      add_markers(
        x = ~lon, y = ~lat, size = ~IPO.Count, key = ~ZIP
        , color = ~IPO.Count, hoverinfo = "text",
        text = ~paste("State: ", data.ipo.count$State, "<br />",
                      "City: ", data.ipo.count$City, "<br />",
                      "ZIP: ", clean.zipcodes(data.ipo.count$ZIP), "<br />",
                      "IPO Count: ", data.ipo.count$IPO.Count)
      ) %>%
      layout(title = "ZIP IPO Count", geo = g)
  })
  
  output$ts <- renderPlotly({
    event.zip <- clean.zipcodes(event_data("plotly_click", source = "map")$key)
    if(is.null(event.zip)){
      return(NULL)
    } else {
      # get zillow data
      if(input$zillow.type == "Zillow Value") {
        data.price <- read.csv(paste0("data/value/", event.zip, ".csv")
                               , stringsAsFactors = FALSE)
        data.price <- cbind(time.value, data.price)
        colnames(data.price) <- c("Time", "Price")
      } else if(input$zillow.type == "SqFt") {
        data.price <- read.csv(paste0("data/sqft/", event.zip, ".csv")
                               , stringsAsFactors = FALSE)
        data.price <- cbind(time.sqft, data.price)
        colnames(data.price) <- c("Time", "Price")
      }
      # get ipo data
      data.ipo <- data.marketCap[data.marketCap$ZIP == event.zip, ]
      if(input$ipo.type == "Yearly Total")
        data.ipo <- data.ipo[-4]
      if(input$ipo.type == "Cummulative")
        data.ipo <- data.ipo[-3]
      
      colnames(data.ipo)[3] = "Value"
      
      # plot
      zip.info <- zipcode[zipcode$zip == event.zip, ]
      plot.title <- paste0(zip.info$city, ", ", zip.info$state, " ", zip.info$zip)
      rm(zip.info)
      
      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = "IPO Market Cap"
      )
      
      plot_ly() %>%
        add_lines(data = data.price, x = ~Time, y = ~Price , name = "Price") %>%
        add_markers(data = data.ipo, x = ~Time, y = ~Value, name = "IPO", yaxis = "y2") %>%
        layout(
          title = plot.title, yaxis2 = ay,
          xaxis = list(title="x")
        )
    }
  })
}