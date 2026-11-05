library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
#source the cleaning function
source("mobileProcessingFunction.r")
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

dfList07 <- list(m1_07Day1, m1_07Day2, 
               m2_07Day1, m2_07Day2)
allDataUnclean07 <- bind_rows(dfList07)
###exectue function for July SUrvey
mobile07Processed <- mobileProcessingFunction(unprocessedMobileData = allDataUnclean07, dayOneDate = dayOneDate)
data <- mobile07Processed$processedMobileData
mobile07Processed$map
write_csv(data, "data/Outputs/202607_MobileData.csv")

# October Survey ----------------------------------------------------------

#time was inferred by the tags and gaps in data
dayOneDate <- ymd_hms("2026-10-13 10:19:00", tz = "America/Denver")
### DAY TWO SURVEY DATE

####M1
m1_10Day1 <- read_csv("data/CR1000 M1_Detection_20261013.dat", skip = 1, 
                      col_types = cols(
                        batt_volt = col_character()
                      )
                      ) %>%
  mutate(Ant = "M1")
##M2 day 1
m2_10Day1 <- read_csv("data/CR1000X M2_Detection_20261013.dat", skip = 1, 
                      col_types = cols(
                        batt_volt = col_character()
                      )
                      )%>%
  mutate(Ant = "M2")

####M1 day 2
m1_10Day2 <- read_csv("data/CR1000 M1_Detection_20261014.dat", skip = 1, 
                      col_types = cols(
                        .default = col_character()
                      
                      )
                      ) %>%
  mutate(Ant = "M1")
##M2 day 2
m2_10Day2 <- read_csv("data/CR1000X M2_Detection_20261014.dat", skip = 1, 
                      col_types = cols(
                        batt_volt = col_character(), 
                        rmcutc = col_character()
                      )
                      ) %>%
  mutate(Ant = "M2")

dfList <- list(m1_10Day1, m1_10Day2, 
               m2_10Day1, m2_10Day2)
allDataUnclean <- bind_rows(dfList)

###exectue function for Oct SUrvey
mobile10Processed <- mobileProcessingFunction(unprocessedMobileData = allDataUnclean, dayOneDate = dayOneDate)
data <- mobile10Processed$processedMobileData
mobile10Processed$map
write_csv(data, "data/Outputs/202610_MobileData.csv")

# Lat Long Check ----------------------------------------------------------
#isolating different instances of same coords
data <- mobile10Processed$processedMobileData
data <- data %>%
  mutate(exact_valueUTM_X = sprintf("%.10f", UTM_X))
countData <- data %>%
  ungroup() %>%
  #count rounds to .1 decmial place due to R console, so even if it's not exact 
  count(UTM_X, UTM_Y)
# length(unique(sprintf("'UTM_Y# length(unique(sprintf("'%s'", data$UTM_X)))
# head(sprintf("%.10f", data$UTM_X[abs(data$UTM_X - 415807) < 1]))



filteredData <- data %>% 
  filter(round(UTM_X, 1) == 415754.1)
#write_csv(filteredData, "JulyGPSCorrection_UTMX_410479.csv")

###reporoject to wgs 84 so we can map
utmCRS <- st_crs(32613) #should be same as +init=epsg:4326

dataInUTMS <- sf::st_as_sf(filteredData, coords = c("UTM_X", "UTM_Y"), crs = utmCRS, remove = FALSE)
#reporject
reproject <- sf::st_transform(dataInUTMS, 4326) %>%
  mutate(
    Long_DD = st_coordinates(.)[, "X"],
    Lat_DD = st_coordinates(.)[, "Y"]
  )

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
  ) %>%
  addCircleMarkers(
    data = reproject,
    lng = ~Long_DD, 
    lat = ~Lat_DD,
    # group = as.character(cat),       # Sets the group name for layer control
    # color = ~color_palette(categoryColumn),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste(
      reproject$Date, "<br>",
      reproject$Time, "<br>",
      reproject$TagID) # Optional popup
  )
map
# dataInWGS842 <- dataInWGS84 %>%
#   mutate(categoryColumn = paste(Date, Ant))
# categories <- unique(dataInWGS842$categoryColumn)
# color_palette <- colorFactor(
#   palette = c("red", "blue", "green", "purple"), # Choose 4 distinct colors
#   domain = categories
# )

# x1 <- data %>%
#   filter(str_detect(415807, UTM_X))
# UTM_X