library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
###### HOW TO USE
# Edit the data sources as needed. It's currently set up for 4 total raw data inputs (.dat files),
# but if more or less are needed, then data can be added/deleted from the list() function below
# meant to be used for one survey at a time: ie not to process a survey from April 2023 and October 2026 at the same time
# After the data is processed in R, you need to manually complete the Pass, River, MobileSite, and Survey fields with help from the maps and timestamps
# GIS is best used to move coordinates if needed


###### RUNNING THE PROCESS
#source the cleaning function; see this function for more detail on what gets cleaned/processed and how
source("mobileProcessingFunction.r")
### Bring in test tags, release site data, and Release file
#used to filter out test tags and add attribute data to the final dataset
#TEST TAGS
testTags <- read_csv("data/testTags.csv", 
                     col_types = cols(TAG = col_character())) %>%
  mutate(cleanTag = str_sub(TAG, 4))
##Release file
Release <- read.csv("./data/WGFP_ReleaseData_Master_20260715.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",7), "numeric", "numeric",rep("character",9))) %>%
  janitor::remove_empty()
#Release Sites
releaseSites <- read_csv("data/releaseSites.csv") %>%
  select(-River)


# July Survey 2026 --------------------------------------------------------

###DAY ONE SURVEY DATE
#Prevents data from before the survey from being included
#time was inferred by the tags and gaps in data
surveyData <- ymd_hms("2026-07-14 10:15:00", tz = "America/Denver")

####M1 day 1
#this was copied from the U drive into the "Data" directory of this R project where it's accessible
#for this example, the colunm names are int he second row of the .dat file, which is why skip = 1 here
m1_07Day1 <- read_csv("data/CR1000 M1_Detection_20260714.dat", skip = 1) %>%
  #adds new caloumn with the value "M1" to keep track of which antenna is which
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
#create list of data: if you need to add/subtract data, make sure this is the place where it happens
dfList07 <- list(m1_07Day1, m1_07Day2, 
               m2_07Day1, m2_07Day2)
#bind together all rows
allDataUnclean07 <- bind_rows(dfList07)
###execute cleaning function for July Survey
mobile07Processed <- mobileProcessingFunction(unprocessedMobileData = allDataUnclean07, dayOneDate = surveyData)
data <- mobile07Processed$processedMobileData
#show map
mobile07Processed$map
#save data in the outputs folder of the project directory
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


# OPTIONAL QAQC CHECK -----------------------------------------------------

#Finding instances of "stacked up" coordinates that are the same
#not ok if the timestamps are multiple minutes apart
data <- mobile10Processed$processedMobileData
#formatting to keep the whole UTM
data <- data %>%
  mutate(exact_valueUTM_X = sprintf("%.10f", UTM_X))
#getting a dataset showing the count of how many times each coordinate occurs in the data
countData <- data %>%
  ungroup() %>%
  #count rounds to .1 decmial place due to R console, so even if it's not exact 
  count(UTM_X, UTM_Y)

#isolating specific UTMs to examine the timestamps 
filteredData <- data %>% 
  filter(round(UTM_X, 1) == 415754.1)

###reproject data back to wgs 84 so we can map
#define crs 
utmCRS <- st_crs(32613) 

#define filteredData (isolated data) as spatial object
dataInUTMS <- sf::st_as_sf(filteredData, coords = c("UTM_X", "UTM_Y"), crs = utmCRS, remove = FALSE)
#reproject
reproject <- sf::st_transform(dataInUTMS, 4326) %>%
  mutate(
    Long_DD = st_coordinates(.)[, "X"],
    Lat_DD = st_coordinates(.)[, "Y"]
  )
#map jus the fitered data to see where the stacked up points are
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
#see exactly where the coordinates are stacked
map
