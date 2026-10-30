#this funciton takes a df of unprocessed mobile data and returns a final processed version minus River, Mobile Site, and Pass (added manually)
#also returns map to aid in qaqc with River, mobileSite etc
#it deletes bad rows, fitlers out test tags, 
mobileProcessingFunction <- function(unprocessedMobileData, dayOneDate, testTags1 = testTags, Release1 = Release, releaseSites1 = releaseSites){
  allData_2 <- unprocessedMobileData %>%
    filter(str_detect(TagID, "^900_\\d{12}"))
  
  ##parsing lat/long 
  allData_latLong <- allData_2 %>%
    mutate(
      #degrees
      Lat_deg = as.numeric(str_sub(rmclatitude, 1, 2)), 
      #minutes; eric just subtracts by 4000, since tis character I'm just taking 3rd character onward
      Lat_min = as.numeric(str_sub(rmclatitude, 3)), 
      #decimal degrees
      Lat_DD = round(Lat_deg + (Lat_min / 60), 6)
    ) %>%
    #long
    mutate(
      Long_deg = abs(as.numeric(str_sub(rmclongitude, 2, 4))), 
      Long_min = abs(as.numeric(str_sub(rmclongitude, 5))),
      Long_DD = -(round(Long_deg + (Long_min/60), 6))
      
    )
  
  ### TImStamp
  allData_latLongTimsetamp <- allData_latLong %>%
    mutate(
      # 1. Convert the original column to a proper datetime format (POSIXct)
      # ymd_hms automatically handles the fractional seconds (.04)
      TIMESTAMP = ymd_hms(TIMESTAMP, tz = "America/Denver"),
      # 
      # # 2. Extract just the date part (Date format)
      # DetDate = as.Date(TIMESTAMP),
      # 
      # # 3. Extract just the time part as a character (or time class)
      DetTime2 = format(round_date(TIMESTAMP, "second"), "%H:%M:%S") 
    ) %>%
    #remove tags before starting
    filter(TIMESTAMP > dayOneDate)
  
  #correct tag # and no dupes
  allData_latLongTimsetampTags2 <- allData_latLongTimsetamp %>%
    #making sure time order is correct, probably redundant
    arrange(Ant, TIMESTAMP) %>%
    mutate(TagID = str_sub(TagID, 5), 
           dupes = if_else(lag(TagID) == TagID, 0, 1, missing = 1)
           #dupes = if_else(lag(TagID) == TagID, 1, 0, missing = 0)
    ) %>%
    filter(dupes == 1)
  #filter out test tags
  allData_latLongTimsetampTagsNoTests <- allData_latLongTimsetampTags2 %>%
    filter(!TagID %in% unique(testTags1$cleanTag))
  
  allData_final <- allData_latLongTimsetampTagsNoTests %>%
    mutate(#Ant = "M1", 
      Pass = NA, 
      River = "Colorado River", 
      MobileSite = NA, 
      Num = NA, 
      Notes = NA, 
      Survey = "Summer 2026", 
      Event = "Detection")
  ## Get columns in right order before reprojecting: this is like "combined data for GIS" sheet in the excel workbook
  allData_final2 <- allData_final %>%
    select(
      Num,
      River,
      MobileSite,
      Date = DetDate,
      #time is the time on the timsetampo not in the native DetTime column
      Time = DetTime2,
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

  
  ## reprojecting coords; need to first make wgs84 sf object
  latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326
  
  dataInWGS84 <- sf::st_as_sf(allData_final2, coords = c("Long_DD", "Lat_DD"), crs = latLongCRS, remove = FALSE)
  #before reprojecting, make map
  ## MAP: used to check crcc etc
  map <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(maxZoom = 19.5)
    ) 
  dataInWGS842 <- dataInWGS84 %>%
    mutate(categoryColumn = paste(Date, Ant))
  categories <- unique(dataInWGS842$categoryColumn)
  color_palette <- colorFactor(
    palette = c("red", "blue", "green", "purple"), # Choose 4 distinct colors
    domain = categories
  )
  for (cat in categories) {
    
    # Filter data for just this category
    df_subset <- dataInWGS842 %>% 
      filter(categoryColumn == cat)
    
    map <- map %>%
      addCircleMarkers(
        data = df_subset,
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
          df_subset$TagID) # Optional popup
      )
  }
  
  map <- map %>%
    addLayersControl(
      overlayGroups = as.character(categories),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  #convert to UTMs
  reproject <- sf::st_transform(dataInWGS84, 32613)
  reproject2 <- reproject %>%
    mutate(
      UTM_X = st_coordinates(.)[, "X"],
      UTM_Y = st_coordinates(.)[, "Y"]
    ) %>%
    st_drop_geometry()
  
  ###Getting attribute info
  attributeINfo <- Release1 %>%
    select(TagID,
           Species,
           Length,
           Weight,
           TagSize,
           ReleaseSite
    ) %>%
    right_join(reproject2, by = "TagID") 
  # adding release site numbers
  # final df columns based on whats in exel sheet mobile_AllData
  attributeINfo2 <- attributeINfo %>%
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
    "processedMobileData" = attributeINfo2,
    "map" = map
  ))
}
