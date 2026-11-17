#this funciton takes a df of unprocessed mobile data and returns a final processed version minus River, Mobile Site, and Pass (added manually)
#also returns map to aid in qaqc with River, mobileSite etc
#it deletes bad rows, fitlers out test tags, 
mobileProcessingFunction <- function(unprocessedMobileData, dayOneDate, testTags1 = testTags, Release1 = Release, releaseSites1 = releaseSites){
  
  # Initial filtering and lat long coord parsing ----------------------------
  parsedCoordsData <- unprocessedMobileData %>%
    # Keep only valid PIT tags matching the standard 15-digit ISO format ("900_" followed by 12 digits)
    filter(str_detect(TagID, "^900_\\d{12}")) %>%
    mutate(
      # Parse NMEA/RMC latitude (formatted as DDMM.MMMM) into Decimal Degrees (DD):
      # Extract the first 2 characters as integer degrees
      Lat_deg = as.numeric(str_sub(rmclatitude, 1, 2)), 
      # Extract everything from the 3rd character onward as decimal minutes
      Lat_min = as.numeric(str_sub(rmclatitude, 3)), 
      # Convert minutes to fractional degrees (divide by 60) and add to degrees
      Lat_DD = round(Lat_deg + (Lat_min / 60), 6),
      
      
      # Parse NMEA/RMC longitude (formatted as -DDDMM.MMMM or WDDDMM.MMMM) into Decimal Degrees:
      # Extract characters 2 through 4 as degrees (skipping the leading minus sign or 'W')
      Long_deg = abs(as.numeric(str_sub(rmclongitude, 2, 4))), 
      # Extract everything from the 5th character onward as decimal minutes
      Long_min = abs(as.numeric(str_sub(rmclongitude, 5))),
      # Combine degrees and fractional minutes, then apply negative sign for Western Hemisphere
      Long_DD = -(round(Long_deg + (Long_min/60), 6))
    )
  

# Timestamp formatting and Date Filtering ---------------------------------

 
  timeFilteredData <- parsedCoordsData %>%
    mutate(
      # Parse string timestamps into POSIXct datetime objects in Mountain Time
      TIMESTAMP = ymd_hms(TIMESTAMP, tz = "America/Denver"),
      # Round timestamps to the nearest whole second and format as HH:MM:SS
      DetTime_clean = format(round_date(TIMESTAMP, "second"), "%H:%M:%S")
    ) %>%
    # Discard any equipment testing detections logged before the official survey start date
    filter(TIMESTAMP > dayOneDate)
  
  #correct tag # and no dupes
  cleanDetections <- timeFilteredData %>%
    #making sure time order is correct, probably redundant
    # but important so lag() compares consecutive pings accurately
    arrange(Ant, TIMESTAMP) %>%
    # grouping by antenna is a safety precaution if the last tag entry of the M1 run is the same as the first in the M2 run, that won't get deleted
    #should be impossible based on how we run our surveys and hor the data is arranged but good to be safe for other surveys
    group_by(Ant) %>%
    # Strip the "900_" prefix (first 4 characters), keeping only the 12-digit unique tag ID
    mutate(TagID = str_sub(TagID, 5), 
           # Flag consecutive duplicate detections: if the current TagID matches the previous row's TagID,
           # mark as 0 (duplicate); otherwise mark as 1 (unique detection). First row defaults to 1.
           dupes = if_else(lag(TagID) == TagID, 0, 1, missing = 1)
    ) %>%
    # Retain only non-consecutive pings
    filter(dupes == 1) %>%
    ungroup()
  #filter out test tags
  cleanDetections2 <- cleanDetections %>%
    filter(!TagID %in% unique(testTags1$cleanTag))

# Standardize Columns for export ------------------------------------------

  allData_final <- cleanDetections2 %>%
    mutate(#Ant = "M1", 
      Pass = NA, 
      #need to be annotated
      River = NA, 
      MobileSite = NA, 
      Num = NA, 
      Notes = NA, 
      Survey = NA, 
      Event = "Detection")
  ## Get columns in right order before reprojecting: this is like "combined data for GIS" sheet in the excel workbook
  allData_final2 <- allData_final %>%
    select(
      Num,
      River,
      MobileSite,
      Date = DetDate,
      #time is the time on the timsetampo not in the native DetTime column
      Time = DetTime_clean,
      T109_C,
      Long_DD,
      Lat_DD,
      TagType,
      TagID,
      Event,
      Ant,
      Pass,
      Survey,
      Notes)

# Reprojecting Coords and making Map --------------------------------------

  
  ## Define the WGS84 geographic coordinate reference system (EPSG:4326)
  latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
  
  # Convert the dataframe into a spatial sf object while keeping Lat/Long as standard columns
  dataInWGS84 <- sf::st_as_sf(allData_final2, coords = c("Long_DD", "Lat_DD"), crs = latLongCRS, remove = FALSE)
  
  # Create a grouping variable combining Date and Antenna for map layer toggling
  dataInWGS842 <- dataInWGS84 %>%
    mutate(categoryColumn = paste(Date, Ant))
  # Extract unique Date-Antenna combinations and assign a distinct color to each
  categories <- unique(dataInWGS842$categoryColumn)
  color_palette <- colorFactor(
    palette = c("red", "blue", "green", "purple"), # Choose 4 distinct colors
    domain = categories
  )
  

  ## MAP: used to check crcc etc
  map <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(maxZoom = 19.5)
    ) 
  
  # Loop through each Date-Antenna group to plot clustered markers on separate toggleable layers
  for (cat in categories) {
    
    # Filter data for just this category
    df_subset <- dataInWGS842 %>% 
      filter(categoryColumn == cat)
    
    map <- map %>%
      addCircleMarkers(
        data = df_subset,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,
          spiderfyDistanceMultiplier = 1.8
        ),
        lng = ~Long_DD, 
        lat = ~Lat_DD,
        group = as.character(cat),       # Sets the group name for layer control
        color = ~color_palette(categoryColumn),
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste(
          df_subset$Date, "<br>",
          df_subset$Time, "<br>",
          df_subset$Lat_DD, "<br>",
          df_subset$Long_DD, "<br>",
          df_subset$TagID) # Optional popup
      )
  }
  # add in interactive layer control using groupings from above
  map <- map %>%
    addLayersControl(
      overlayGroups = as.character(categories),
      options = layersControlOptions(collapsed = FALSE)
    )

# Reproject Coordinates to UTMs -------------------------------------------

  ## Reproject spatial coordinates from WGS84 (Lat/Long) to NAD83 / WGS84 UTM Zone 13N (EPSG:32613)
  datainUTMs <- sf::st_transform(dataInWGS84, 32613)
  datainUTMs2 <- datainUTMs %>%
    mutate(
      UTM_X = st_coordinates(.)[, "X"],
      UTM_Y = st_coordinates(.)[, "Y"]
    ) %>%
    st_drop_geometry()
  
  ###Getting attribute info
  finalProcessedData <- Release1 %>%
    select(TagID,
           Species,
           Length,
           Weight,
           TagSize,
           ReleaseSite
    ) %>%
    right_join(datainUTMs2, by = "TagID") 
  # adding release site numbers
  # final df columns based on whats in exel sheet mobile_AllData
  finalProcessedData2 <- finalProcessedData %>%
    left_join(releaseSites1, by = "ReleaseSite") %>%
    select(
      Num,
      River,	MobileSite,	Date,	Time,	T109_C,	UTM_X,	UTM_Y,	TagType,	TagID,	Event,	Ant,	Pass,	Species,	Length,
      Weight,
      TagSize,
      RS_Num,
      ReleaseSite,
      Survey,
      Notes
    ) %>%
    arrange(Ant, Date, Time)
  
  return(list(
    "processedMobileData" = finalProcessedData2,
    "map" = map
  ))
}
