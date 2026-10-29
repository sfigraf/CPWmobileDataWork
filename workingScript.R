library(tidyverse)
library(sf)
#test tags

testTags <- read_csv("data/testTags.csv", 
                     col_types = cols(TAG = col_character())) %>%
  mutate(cleanTag = str_sub(TAG, 4))
###DAY ONE SURVEY DATE
#time was inferred by the tags and gaps in data
dayOneDate <- ymd_hms("2026-07-14 10:15:00", tz = "America/Denver")
### DAY TWO SURVEY DATE

####M1
m1_07Day1 <- read_csv("data/CR1000 M1_Detection_20260714.dat", skip = 1) %>%
  mutate(Ant = "M1")
##M2 day 1
m2_07Day1 <- read_csv("data/CR1000X M2_Detection_20260714.dat", skip = 1)%>%
  mutate(Ant = "M2")

####M1 day 2
m1_07Day2 <- read_csv("data/CR1000 M1_Detection_20260715.dat", skip = 1) %>%
  mutate(Ant = "M1")
##M2 day 2
m2_07Day2 <- read_csv("data/CR1000X M2_Detection_20260715.dat", skip = 1) %>%
  mutate(Ant = "M2")

dfList <- list(m1_07Day1, m1_07Day2, 
               m2_07Day1, m2_07Day2)
allDataUnclean <- bind_rows(dfList)

names(allDataUnclean)
allData_2 <- allDataUnclean %>%
  filter(str_detect(TagID, "^900_\\d{12}"))
#QAQC
deletedRows <- allDataUnclean %>%
  anti_join(allData_2)

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
    # DetTime = format(TIMESTAMP, "%H:%M:%S") 
  ) %>%
  #remove tags before starting
  filter(TIMESTAMP > dayOneDate)

#correct tag # and no dupes
allData_latLongTimsetampTags <- allData_latLongTimsetamp %>%
  #making sure time order is correct, probably redundant
  arrange(Ant, TIMESTAMP) %>%
  mutate(TagID = str_sub(TagID, 5), 
         dupes = if_else(lag(TagID) == TagID, 1, 0, missing = 0)
  ) %>%
  filter(dupes != 1) #%>%
  #select(-dupes)

#filtering out test tags
allData_latLongTimsetampTagsNoTests <- allData_latLongTimsetampTags %>%
  filter(!TagID %in% unique(testTags$cleanTag))

allData_final <- allData_latLongTimsetampTagsNoTests %>%
  mutate(#Ant = "M1", 
         Pass = NA, 
         River = "Colorado River", 
         MobileSite = NA, 
         Num = NA, 
         Notes = NA, 
         Survey = "Summer 2026", 
         Event = "Detection")
## Get columns in right order
allData_final2 <- allData_final %>%
  select(
    Num,
    River,
    MobileSite,
    Date = DetDate,
    Time = DetTime,
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

##checking with already done data
combinedGISdatatoCheck <- read_csv("data/combinedGISdatatoCheck.csv")

## reprojecting coords
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326

dataInWGS84 <- sf::st_as_sf(allData_final2, coords = c("Long_DD", "Lat_DD"), crs = latLongCRS, remove = FALSE)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  addAwesomeMarkers(data = dataInWGS84,
                    #clusterOptions = markerClusterOptions(),
                    label = paste(dataInWGS84$Date, "\n",
                                  dataInWGS84$Time, "\n",
                                  dataInWGS84$TagID),
                    popup = paste(dataInWGS84$Ant),
                    group = "Antennas")
#condensedEventsSF <- sf::st_as_sf(condensedEventsFiltered, coords = c("UTM_X", "UTM_Y"), crs = 32613)
#convert to lat/long
reproject <- sf::st_transform(dataInWGS84, 32613)
reproject2 <- reproject %>%
  mutate(
    UTM_X = st_coordinates(.)[, "X"],
    UTM_Y = st_coordinates(.)[, "Y"]
  ) %>%
  st_drop_geometry()
