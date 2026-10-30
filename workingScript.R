library(tidyverse)
library(sf)
library(janitor)

### these data should be the same for any survey
#test tags
testTags <- read_csv("data/testTags.csv", 
                     col_types = cols(TAG = col_character())) %>%
  mutate(cleanTag = str_sub(TAG, 4))

Release <- read.csv("./data/WGFP_ReleaseData_Master_20260715.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",7), "numeric", "numeric",rep("character",9))) %>%
  janitor::remove_empty()
releaseSites <- read_csv("data/releaseSites.csv") %>%
  select(-River)
  #distinct(RS_Num, .keep_all = TRUE)
#write_csv(releaseSites, "releaseSites.csv")
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

x <- ymd_hms("2026-07-14 15:12:20.886")
round_date(x, "second")
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
  filter(dupes == 1) #%>%
  #select(-dupes)
####### qaqc
check <- allData_latLongTimsetampTags %>%
  select(TIMESTAMP, TagID, Date = DetDate, DetTime, Time = DetTime2)


#filtering out test tags
allData_latLongTimsetampTagsNoTests <- allData_latLongTimsetampTags2 %>%
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

##checking with already done data
combinedGISdatatoCheck <- read_csv("data/combinedGISdatatoCheck.csv")
x<- combinedGISdatatoCheck %>%
  select(TagID, Date, Time, Lat_DD, Long_DD) %>%
  mutate(Date = as.character(mdy(Date)), 
         Time = as.character(Time))
checkFinal <- allData_final2 %>%
  mutate(TagID = as.numeric(TagID)) %>%
  select(TagID, Date, Time, Lat_DD, Long_DD) %>%
  left_join(x, by =c("TagID", "Date", "Time")) %>%
  mutate(latCheck = Lat_DD.x - Lat_DD.y, 
         longCheck = Long_DD.x- Long_DD.y)

## reprojecting coords
latLongCRS <- st_crs("+proj=longlat +datum=WGS84 +no_defs") #should be same as +init=epsg:4326

dataInWGS84 <- sf::st_as_sf(allData_final2, coords = c("Long_DD", "Lat_DD"), crs = latLongCRS, remove = FALSE)

## MAP: can also be used to check crcc etc
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
map
# %>%
#   addAwesomeMarkers(data = dataInWGS84,
#                     clusterOptions = markerClusterOptions(),
#                     label = paste(dataInWGS84$Ant),
#                     popup = paste(
#                                   dataInWGS84$Date, "<br>",
#                                   dataInWGS84$Time, "<br>",
#                                   dataInWGS84$TagID),
#                     group = "Antennas")
#condensedEventsSF <- sf::st_as_sf(condensedEventsFiltered, coords = c("UTM_X", "UTM_Y"), crs = 32613)
#convert to lat/long
reproject <- sf::st_transform(dataInWGS84, 32613)
reproject2 <- reproject %>%
  mutate(
    UTM_X = st_coordinates(.)[, "X"],
    UTM_Y = st_coordinates(.)[, "Y"]
  ) %>%
  st_drop_geometry()

###Getting attribute info
attributeINfo <- Release %>%
  select(TagID,
         Species,
         Length,
         Weight,
         TagSize,
         ReleaseSite
    ) %>%
  right_join(reproject2, by = "TagID") 

attributeINfo2 <- attributeINfo %>%
  left_join(releaseSites, by = "ReleaseSite") %>%
  select(
    Num,
    River,	MobileSite,	Date,	Time,	T109_C,	UTM_X,	UTM_Y,	TagType,	TagID,	Event,	Ant,	Pass,	Species,	Length,
    Weight,
    TagSize,
    RS_Num,
    ReleaseSite,
    Survey,
    Notes
    
  )

mobile07Processed <- mobileProcessingFunction(unprocessedMobileData = allDataUnclean, dayOneDate = dayOneDate)
data <- mobile07Processed$processedMobileData
write_csv(attributeINfo2, "data/Outputs/202607_MobileData.csv")
