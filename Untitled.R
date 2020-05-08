library(dplyr) # data.frames
library(sf)    # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables

# Shiny
library(shiny)    # Starting Reactivity

# 2. Load data ----
counties <- readRDS("data/counties.rds")

# Source helper functions -----
source("helpers.R")

# 3. Initalize data
covid19_data <-  read_covid19()
today        <-  today_centroids(counties, covid19_data)
leaf_map     <-  base_leaf(today)


ui <- fluidPage(
  # Sidebar layout output definitions ----
  sidebarPanel(
    tags$script(' $(document).on("keydown", function (e) {Shiny.onInputChange("lastkeypresscode", e.keyCode);});'),
    autocomplete_input("auto", "Search for a County:", structure(today$fips, names = today$call)),
    # Output: Message ----
    textOutput("covid_message", container = h3),
    DTOutput("covid_table")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # App title ----
    titlePanel('Mike Johnson: COVID-19 Tracker'),
    # Output: Map ----
    leafletOutput('covid_map'),
    # Output: Chart ----
    dygraphOutput('covid_chart')
  )
)


server <- function(input, output, session) {
  
  # Global variables initialized ----
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = " First Shiny!!! ")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covid_map     <- renderLeaflet({ leaf_map })
  
  # dyGraph Chart ----
  # ---- must be rendered as dyGraph ----
  output$covid_chart   <- renderDygraph({ make_chart(covid19_data, FIP) })
  
  # DT Table ----
  # ---- must be rendered as DT ----
  output$covid_table   <- renderDT({ make_DT(today, FIP) })
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covid_map_marker_mouseover, {
    txt = filter(today,fips == input$covid_map_marker_mouseover$id) 
    v$msg <- paste("Mouse is over: ", txt$call)
  })
  
  observeEvent(input$covid_map_marker_mouseout, { v$msg <- "Mouse is over: " })
  
  observeEvent(input$covid_map_marker_click, {
    FIP <<- input$covid_map_marker_click$id
    output$covid_chart <- renderDygraph({ make_chart(covid19_data, FIP) })
    leafletProxy('covid_map') %>% zoom_to_county(counties, FIP)
    output$covid_table   <- renderDT({ make_DT(today, FIP) })
  })
  
  observe( 
    if(input$auto == "" || is.null(input$auto)){ 
      NULL 
    } else {
        FIP <<- input$auto
        leafletProxy("covid_map") %>% zoom_to_county(counties, FIP)
        output$covid_chart <- renderDygraph({ make_chart(covid19_data, FIP) })
        output$covid_table      <- renderDT({ make_DT(today, FIP) })
    }
  )
  
  # Message to Display ----
  # ---- must be rendered as text ----
  output$covid_message <- renderText(v$msg)
}

runApp(shinyApp(ui, server), launch.browser = TRUE)