# Source helper functions -----
source("helpers.R")
library(shiny)

# Initalize data
covid19  <-  read_covid19('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)

# User interface ----
ui <- fluidPage( 
    titlePanel('Mike Johnson: COVID-19 Dashboard')
)

# Server logic ----
server <- function(input, output) {
    
}

shinyApp( ui, server )
