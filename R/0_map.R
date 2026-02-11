
library(leaflet)
library(sf)
library(tidyverse)

df_locations <- data.frame(
  area = c("Lombok", "Sumbawa", "Manado", "Sorong"),
  latitude  = c(-8.5833, -8.4932,  1.4748, -0.8762),
  longitude = c(116.1167, 117.4202, 124.8421, 131.2558)

)

world <- sf::st_read(
  "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",
  quiet = TRUE
)

leaflet(df_locations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = world,
    fillColor = "white",
    fillOpacity = 0.01,
    color = NA,
    opacity = 0
  ) %>%
  addCircleMarkers(
    data = df_locations,
    lng = ~longitude,
    lat = ~latitude,
    label = ~area,
    labelOptions = labelOptions(
      noHide = TRUE,
      # textOnly = TRUE,
      style = list("font-weight" = "bold")
    ),
    radius = 7,
    color = "maroon",
    fillOpacity = 0.9
  )




# option 2
leaflet(df_locations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~area,
    labelOptions = labelOptions(
      noHide = TRUE,
      # textOnly = TRUE,
      style = list("font-weight" = "bold"),
      textsize = "30px",
      direction = "top",
      offset = c(0, 0),
    ),
    radius = 7,
    color = "maroon",
    fillOpacity = 0.9
  )





# Labeling the flags
Labell <- paste(
  'Region: ', SouthWest_isod_GPS_ALL$ADMIN2_NAME, '(', South_west_GPS_ALL$LocationName, ')','<br/>',
  'Year: ', SouthWest_isod_GPS_ALL$Year, '<br/>',
  'Prev: ', SouthWest_isod_GPS_ALL$Prev_label, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

# Create a leaflet map
map <- leaflet(BF_spdf_sf) %>%
  # Add tiles
  addTiles() %>% 
  setView( lat=12.3710, lng=-1.5197, zoom=7) %>%
  addPolygons( stroke = F,
               fillOpacity = 0.3,
               fillColor = "white") %>% 
  # Add markers for each point
  addCircleMarkers(
    ~South_west_GPS_ALL$Longitude, ~South_west_GPS_ALL$Latitude,
    label = Labell,
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep labels always visible
      direction = "bottom",
      textOnlyIfOverlapping = TRUE
    ),
    color = color,
    popup = ~paste("Latitude:", round(SouthWest_isod_GPS_ALL$Latitude, 6), "<br>",
                   "Longitude:", round(SouthWest_isod_GPS_ALL$Longitude, 6))
  ) %>% 
  # Add a tile layer for the world with white color
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1, maxZoom = 18)) %>% 
  # Add a geojson layer for the world with white fill color
  addGeoJSON("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",
             fillColor = "white", fillOpacity = 1, color = "white", weight = 1)

# Display the map
map




var2b <- leaflet(df_locations) %>%
  addTiles() %>%
  addCircleMarkers(
  lng = ~longitude,
  lat = ~latitude,
    # label = ~area,
    labelOptions = labelOptions(
      # noHide = TRUE,
      # textOnly = TRUE,
      style = list("font-weight" = "bold"),
      textsize = "20px",
      direction = "top",
      offset = c(0, 0),
    ),
  radius = 7,
  color = "maroon",
  fillOpacity = 0.9
  ) %>% 
  addMarkers(
    data = df_locations,
    lng = ~longitude,
    lat = ~latitude
  )

saveas(m, "/path/to/folder/index.html")