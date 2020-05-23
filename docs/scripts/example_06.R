# Source helper functions -----
source("helpers.R")

# Initalize data
covid19  <-  read_covid19()
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)

# User interface ----
ui <- fluidPage( 
  titlePanel('Mike Johnson: COVID-19 Dashboard'),
  
  # Sidebar layout output definitions ----
  sidebarPanel(
    # Output: Message ----
    textOutput("covidMessage", container = h4)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Map ----
    leafletOutput('covidMap')
  )
)

# Server logic ----
server <- function(input, output) {
  # Global variables initialized ----
  v   <- reactiveValues(msg = "First Shiny!!!")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covidMap     <- renderLeaflet({ basemap })
  # Message to Display ----
  # ---- must be rendered as text ----
  output$covidMessage <- renderText(v$msg)
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covidMap_marker_mouseover, {
    txt = filter(today, fips == input$covidMap_marker_mouseover$id) 
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })
  
  observeEvent(input$covidMap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })
  
}

shinyApp( ui, server )
