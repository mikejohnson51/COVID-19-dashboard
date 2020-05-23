# 2. Load packages ----
# Data Manipulation
library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables
library(rvest)    # webscraping

# Shiny
library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(shinythemes) # themes

# Load Spatial Data

counties = readRDS("./data/counties.rds")

# Read in COVID-19 Timesries from URL
read_covid19 = function(url){
  url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  read.csv(url, stringsAsFactors = FALSE) %>% 
    mutate(date = as.Date(date), 
           fips = as.numeric(fips), 
           name = paste0(county, " County, ", state))
}

# Join County Data with 
today_centroids = function(counties, covid_data){
  filter(covid_data, date == max(date)) %>% 
    left_join(st_centroid(counties), by = 'fips') %>% 
    na.omit() %>% 
    mutate(size = abs(cases - mean(cases)) / sd(cases)) %>% 
    st_as_sf()
}

make_graph  = function(covid19, FIP){
  
  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date
  
  # Fit and exponetial model
  exponential.model <- lm(log(cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))
  
  dygraph(data = select(subset, cases, deaths, expCases),
          main = paste0("COVID-19 Trend: ", subset$name[1]),
          ylab = 'Number of Cases/Deaths', 
          xlab = 'Date') %>% 
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.2,
                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
    #dyOptions(colors = c("blue", "red", "black"))
    dyOptions(colors = c("#003660", "#FEBC11", "black"))
  
}

basemap = function(today){
  pal <- colorNumeric("inferno", domain  = today$size, n = 50)
  pal2 <- colorNumeric("inferno", domain = today$cases, n = 50)
  
  leaflet(data = today) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addScaleBar("bottomleft") %>%
    addCircleMarkers(
      fillColor   = ~pal(size), 
      color       = 'transparent', 
      fillOpacity = 0.5,
      radius      = ~size*2,
      layerId     = ~fips,
      label       = ~name ) %>% 
    addLegend("bottomright", pal = pal2, values = ~cases,
              title = paste("COVID Cases\n", max(today$date)),
              opacity = 1)
}

zoom_to_county = function(map, counties, FIP){
  
  shp = filter(counties, fips == FIP) 
  bounds = shp %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_buffer(.1) %>% 
    st_bbox() %>% 
    as.vector()
  
  clearShapes(map) %>% 
    addPolygons(data = shp,
                color = "#003660", 
                fillColor  = "#FEBC11", 
                fillOpacity = .2) %>% 
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

## Scrape Wikipeida
make_table = function(today, FIP){
  myfips = filter(today, fips == FIP)
  
  url = paste0('https://en.wikipedia.org/wiki/',  gsub(" ", "_", myfips$name)) %>% 
    read_html() %>%
    html_nodes("table.infobox") %>% 
    html_table(fill= TRUE) 
  
  l = url[[1]]
  ll = l[!l[,1] == l[,2],]
  
  ## Make a datatable from the resulting data.frame and turn off paging
  datatable(ll,  caption = paste('Wikipedia Information:', myfips$name), 
            options = list(paging = TRUE, searching = FALSE, ordering = FALSE),
            colnames = rep("", ncol(ll)))
}

## Build State Ranking table
make_table2 = function(today, FIP){
  myfips = filter(today, fips == FIP)
  
  # Filter todays data to the state of the input FIP
  mydata = filter(today, state == myfips$state) %>% 
    # Arrange the cases from largest to smallest
    arrange(desc(cases)) %>% 
    # Drop the geometry column
    st_drop_geometry() %>% 
    # Keep the  County name, cases and deaths
    select(County = county, Cases = cases, Deaths = deaths) %>% 
    # Create a new variable called Death Rate
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) %>% 
    head(10)
  
  
  # Make an interactive Table! with a caption
  datatable(mydata, caption = paste('COVID-19 Statistics', myfips$state, myfips$date), options = list(paging = FALSE, searching = FALSE)) 
}

