source("helpers.R")

# 3. Initalize data
covid19  <-  read_covid19('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)


ui <- fluidPage(
  titlePanel('Mike Johnson: COVID-19 Tracker'),
  # Sidebar layout output definitions ----
  sidebarPanel(
    textOutput("covid_message", container = h3),
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Map ----
    leafletOutput('covidmap',   height = '600px'),
    # Output: Chart ----
    dygraphOutput('covid_graph', height = '600px')
  )
)

server <- function(input, output, session) {
  # Global variables initialized ----
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "COVID-Tracker")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covidmap     <- renderLeaflet({ basemap })
  
  # dyGraph Chart ----
  # ---- must be rendered as dyGraph ----
  output$covid_graph   <- renderDygraph({ make_graph(covid19, FIP) })
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covidmap_marker_mouseover, {
    txt = filter(today, fips == input$covidmap_marker_mouseover$id) 
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })
  
  observeEvent(input$covidmap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })
  
  observeEvent(input$covidmap_marker_click, {
    FIP <<- input$covidmap_marker_click$id
    output$covid_graph <- renderDygraph({ make_graph(covid19, FIP) })
    leafletProxy('covidmap') %>% zoom_to_county(counties, FIP)
  })
  
  # Message to Display ----
  # ---- must be rendered as text ----
  output$covid_message <- renderText(v$msg)
}

shinyApp(ui, server)