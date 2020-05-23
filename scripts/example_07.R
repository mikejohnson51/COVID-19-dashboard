source("helpers.R")

# 3. Initalize data
covid19  <-  read_covid19()
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)


ui <- fluidPage(
  titlePanel('Mike Johnson: COVID-19 Tracker'),
  # Sidebar layout output definitions ----
  sidebarPanel(
    textOutput("covidMessage", container = h3),
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Map ----
    leafletOutput('covidMap'),
    # Output: Chart ----
    dygraphOutput('covidGraph')
  )
)

server <- function(input, output, session) {
  # Global variables initialized ----
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "COVID-Tracker")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covidMap     <- renderLeaflet({ basemap })
  
  # dyGraph Chart ----
  # ---- must be rendered as dyGraph ----
  output$covidGraph   <- renderDygraph({ make_graph(covid19, FIP) })
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covidMap_marker_mouseover, {
    txt = filter(today, fips == input$covidMap_marker_mouseover$id) 
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })
  
  observeEvent(input$covidMap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })
  
  observeEvent(input$covidMap_marker_click, {
    FIP <<- input$covidMap_marker_click$id
    output$covidGraph <- renderDygraph({ make_graph(covid19, FIP) })
    leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
  })
  
  # Message to Display ----
  # ---- must be rendered as text ----
  output$covidMessage <- renderText(v$msg)
}

shinyApp(ui, server)
