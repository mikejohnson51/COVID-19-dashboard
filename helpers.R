read_covid19 = function(url){
  read.csv(url, stringsAsFactors = FALSE) %>% 
    mutate(date = as.Date(date), 
           fips = as.numeric(fips), 
           name = paste0(county, " County, ", state))
}

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
  pal <- colorNumeric("viridis", NULL, n = 100)
  
  leaflet(data = today) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addScaleBar("bottomleft") %>%
    addCircleMarkers(
      fillColor = ~pal(size), 
      color = 'transparent', 
      fillOpacity = 0.5,
      radius = ~size,
      layerId = ~fips,
      label =~fips )
}

zoom_to_county = function(m, data, FIP){
  
  shp = filter(data, fips == FIP) 
  bounds = shp %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_buffer(.1) %>% 
    st_bbox() %>% 
    as.vector()
  
  clearShapes(m) %>% 
    addPolygons(data = shp,
                color = "red", fillColor  = "white", fillOpacity = .4) %>% 
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