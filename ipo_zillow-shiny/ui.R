# Author: Sam Turner
# Script for doing data exploration on IPO data
# Will make maps, etc...

# LIBRARIES
library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("IPO Count by ZIP"),
  plotlyOutput("plot"),
  radioButtons("zillow.type", "Zillow Data", 
               c("Zillow Value", "SqFt")),
  radioButtons("ipo.type", "IPO Data",
               c("Yearly Total", "Cummulative")),
  #textInput("zip.input", "ZIP", "85228", width = "25%"),
  plotlyOutput("ts")
)