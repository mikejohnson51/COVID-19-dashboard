source("helpers.R")

# 3. Initalize data
covid19  <-  read_covid19()
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)

ui <- fluidPage(
  titlePanel("Mike Johnson: COVID-19 Dashboard"),
  # Sidebar layout output definitions ----
  sidebarPanel(
    # Output: Message ----
    DTOutput("covidTable"),
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    textOutput("covidMessage", container = h4),
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
  
  # DT Table ----
  # ---- must be rendered as DT ----
  output$covidTable   <- renderDT({ make_table(today, FIP) })
  
  # Events ----
  # ---- mouse ----
  observeEvent(input$covidMap_marker_mouseover, {
    txt = filter(today, fips == input$covidMap_marker_mouseover$id) 
    v$msg <- paste0("Mouse is over: ", txt$name, " (", prettyNum(txt$cases, big.mark=","), " cases)")
  })
  
  observeEvent(input$covidMap_marker_mouseout, { v$msg <- "Mouse is over: " })
  
  observeEvent(input$covidMap_marker_click, {
    FIP <<- input$covidMap_marker_click$id
    output$covidGraph <- renderDygraph({ make_graph(covid19, FIP) })
    leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
    output$covidTable   <- renderDT({ make_table(today, FIP) })
  })

  # Message to Display ----
  # ---- must be rendered as text ----
  output$covidMessage <- renderText(v$msg)
}

shinyApp(ui, server)
