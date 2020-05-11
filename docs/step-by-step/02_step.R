# Source helper functions -----
source("helpers.R")
library(shiny)

# Initalize data
covid19  <-  read_covid19('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)

# User interface ----
ui <- fluidPage( 
  titlePanel('Mike Johnson: COVID-19 Dashboard'),
  
  # Sidebar layout output definitions ----
  sidebarPanel(
    # Output: Message ----
    textOutput("covid_message", container = h4)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Map ----
    leafletOutput('covidmap')
  )
)

# Server logic ----
server <- function(input, output) {
  # Global variables initialized ----
  v   <- reactiveValues(msg = "First Shiny!!!")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covidmap     <- renderLeaflet({ basemap })
  # Message to Display ----
  # ---- must be rendered as text ----
  output$covid_message <- renderText(v$msg)
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covidmap_marker_mouseover, {
    txt = filter(today, fips == input$covidmap_marker_mouseover$id) 
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })
  
  observeEvent(input$covidmap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })
  
}

shinyApp( ui, server )
