library(tidyverse)
#test tags

testTags <- read_csv("data/testTags.csv", 
                     col_types = cols(TAG = col_character())) %>%
  mutate(cleanTag = str_sub(TAG, 4))
###DAY ONE SURVEY DATE
#time was inferred by the tags and gaps in data
dayOneDate <- ymd_hms("2026-07-14 10:15:00", tz = "America/Denver")
### DAY TWO SURVEY DATE

####M1
m1_07Day1 <- read_csv("data/CR1000 M1_Detection_20260714.dat", skip = 1)
names(m1_07Day1)
m1_07Day1_2 <- m1_07Day1 %>%
  filter(str_detect(TagID, "^900_\\d{12}"))
#QAQC
deletedRows <- m1_07Day1 %>%
  anti_join(m1_07Day1_2)

##parsing lat/long 
m1_07Day1_latLong <- m1_07Day1_2 %>%
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
m1_07Day1_latLongTimsetamp <- m1_07Day1_latLong %>%
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
m1_07Day1_latLongTimsetampTags <- m1_07Day1_latLongTimsetamp %>%
  #making sure time order is correct, probably redundant
  arrange(TIMESTAMP) %>%
  mutate(TagID = str_sub(TagID, 5), 
         dupes = if_else(lag(TagID) == TagID, 1, 0, missing = 0)
  ) %>%
  #using != 1 because there's NAs in there too
  filter(dupes != 1) %>%
  select(-dupes)

#filtering out test tags
m1_07Day1_latLongTimsetampTagsNoTests <- m1_07Day1_latLongTimsetampTags %>%
  filter(!TagID %in% unique(testTags$cleanTag))

m1_07Day1_final <- m1_07Day1_latLongTimsetampTagsNoTests %>%
  mutate(Ant = "M1", 
         Pass = 1, 
         River = "Colorado River", 
         MobileSite = NA, 
         Num = NA, 
         Notes = NA, 
         Survey = "Summer 2026", 
         Event = "Detection")
## Get columns in right order
m1_07Day1_final2 <- m1_07Day1_final %>%
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

