# 1. Install missing packages ----
list.of.packages <- c("dplyr", "sf", "leaflet", "dygraphs", "DT", 'shiny')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# 2. Load packages ----
# Data Manipulation
library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables

# Shiny
library(shiny)    # Starting Reactivity

# Load Spatial Data

counties = readRDS("../data/counties.rds")

# Read in COVID-19 Timesries from URL
read_covid19 = function(url){
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

make_graph  = function(data, FIP){
  subset = filter(data, fips == FIP)
  rownames(subset) <- as.Date(subset$date)
  dplyr::select(subset, cases, deaths) %>% 
    dygraph(
      main = paste0("COVID-19 Trend: ", 
                    paste(subset$county[1], "County,", subset$state[1])),
      ylab = 'Number of Cases/Deaths', xlab = 'Date') %>% 
    dyOptions(stackedGraph = TRUE)
}


basemap = function(today){
  pal <- colorNumeric("inferno", domain = today$size, n = 50)
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
                color = "red", 
                fillColor  = "white", 
                fillOpacity = .2) %>% 
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}
make_table = function(data, FIP){
  event = data %>% filter(fips == FIP)
  url = paste0('https://en.wikipedia.org/wiki/',  gsub(" ", "_", event$call))
  links <- read_html(url) %>%
    html_nodes("table.infobox") %>% 
    html_table(fill= TRUE) 
  
  DT::datatable(links[[1]], options = list(paging = FALSE))
}