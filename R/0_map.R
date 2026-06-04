
library(leaflet)
library(sf)
library(tidyverse)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::right_join(
    read.table("outputs/result_poppunk/qfile_filtered_19to23.txt") %>% 
      dplyr::mutate(specimen_id = V1,
                    workPoppunk_qc = "pass_qc") %>% 
      dplyr::select(specimen_id, workPoppunk_qc)
    ,
    by = "specimen_id"
    
  ) %>% 
  dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  glimpse()

df_dummy <- df_epi_gen_pneumo %>% 
  dplyr::group_by(area, serotype_final_decision) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::group_by(area) %>% 
      dplyr::summarise(n_area = n(), .groups = "drop")
    ,
    by = "area"
  ) %>% 
  
  # VT-NVT label
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::distinct(serotype_final_decision, .keep_all = TRUE
                      ) %>% 
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision)
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::mutate(
    percent = n/n_area*100,
    report = paste0(round(percent, 2), "% (",
                    round(n, 2), "/",
                    round(n_area, 2), ")"
                    ),
    latitude = case_when(
      area == "Lombok" ~ -8.5833,
      area == "Sumbawa" ~ -8.4932,
      area == "Minahasa" ~ 1.4748,
      area == "Sorong" ~ -0.8762,
      TRUE ~ NA_real_
    ),
    longitude = case_when(
      area == "Lombok" ~ 116.1167,
      area == "Sumbawa" ~ 117.4202,
      area == "Minahasa" ~ 124.8421,
      area == "Sorong" ~ 131.2558,
      TRUE ~ NA_real_
    )
  ) %>% 
  glimpse()

df_grouped_report <- dplyr::bind_rows(
  # VT
  df_dummy %>% 
    dplyr::filter(area == "Lombok" &
                    serotype_classification_PCV13_final_decision == "VT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sumbawa" &
                    serotype_classification_PCV13_final_decision == "VT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Minahasa" &
                    serotype_classification_PCV13_final_decision == "VT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sorong" &
                    serotype_classification_PCV13_final_decision == "VT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  # NVT
  df_dummy %>% 
    dplyr::filter(area == "Lombok" &
                    serotype_classification_PCV13_final_decision == "NVT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sumbawa" &
                    serotype_classification_PCV13_final_decision == "NVT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Minahasa" &
                    serotype_classification_PCV13_final_decision == "NVT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sorong" &
                    serotype_classification_PCV13_final_decision == "NVT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  # NT
  df_dummy %>% 
    dplyr::filter(area == "Lombok" &
                    serotype_classification_PCV13_final_decision == "NT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sumbawa" &
                    serotype_classification_PCV13_final_decision == "NT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Minahasa" &
                    serotype_classification_PCV13_final_decision == "NT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
  df_dummy %>% 
    dplyr::filter(area == "Sorong" &
                    serotype_classification_PCV13_final_decision == "NT") %>% 
    dplyr::slice_max(order_by = percent, n = 3) %>%
    ungroup()
  ,
) %>% 
  glimpse()
  







df_locations <- data.frame(
  area = c("Lombok", "Sumbawa", "Minahasa", "Sorong"),
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


# option 3
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
  'VT: ', SouthWest_isod_GPS_ALL$Year, '<br/>',
  'NVT: ', SouthWest_isod_GPS_ALL$Prev_label, '<br/>', 
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
