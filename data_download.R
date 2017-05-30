library(knitr)
library(tidyverse)
library(lubridate)
library(RCurl)
library(data.table)
library(stringr)
library(R.utils)

#create download and save-file URLs

## To analyze most recent data uncomment the following two lines
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
allfiles <-  getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  strsplit("\r\n") %>%
  unlist()

## To speed up for testing, ftp docs to "data/" and uncomment the next two lines
## 
# url <- "data/"
# filenames <- dir("data/")

dld_base <- "StormEvents_details"

if (!file.exists("data")) {
        dir.create("data")
}

if (!exists("weather_events")) {
  # If not, get to work
  # detail.files contains the list of the files that we actually
  # want to download. In this case 1997 - 2017
  
  detail.files <- allfiles[grep(dld_base, allfiles)]
  filenames = vector(length = 21)
  for (i in 1997:2017) {
    filenames[i - 1996] <-
      detail.files[grep(paste0(".*_d", i, "_.*"), detail.files)]
  }
  
  # read files into Weather Event Data Frame (wedf)
  weather_events <- data.table()
  for (files in filenames) {
     print(files)
    # Check if the file has been downloaded and unzipped
    # If not, download, unzip, and store the extracted filename
    # Otherwise, just save the 
    if(! file.exists(paste0("data/", str_replace(files, "\\.gz$", "")))) {
    download.file(url = paste0(url, files)
                  , destfile = paste0("data/", files))
    t <-  gunzip(filename = paste0("data/", files), temporary = FALSE)
    } else t <- paste0("data/", str_replace(files, "\\.gz$", ""))
      weather_events <- rbind(weather_events,
            fread(t ,
            select = c("EPISODE_ID", "EVENT_ID", "EVENT_TYPE"
                       , "BEGIN_DATE_TIME", "END_DATE_TIME","CZ_TIMEZONE" 
                       , "STATE_FIPS", "CZ_FIPS", "WFO"
                       , "INJURIES_DIRECT", "INJURIES_INDIRECT"
                       , "DEATHS_DIRECT", "DEATHS_INDIRECT"
                       , "DAMAGE_PROPERTY", "DAMAGE_CROPS"
                       , "MAGNITUDE", "MAGNITUDE_TYPE", "FLOOD_CAUSE"
                       , "CATEGORY", "TOR_F_SCALE", "TOR_LENGTH", "TOR_WIDTH"
                       , "BEGIN_RANGE", "BEGIN_AZIMUTH","BEGIN_LOCATION"
                       , "END_RANGE","END_AZIMUTH","END_LOCATION"
                       , "BEGIN_LAT", "BEGIN_LON", "END_LAT","END_LON"
                       , "EPISODE_NARRATIVE","EVENT_NARRATIVE")
            )
      ) 

  }
  
}
#make names lowercase)   

names(weather_events) <- tolower(names(weather_events))

weather_events <- weather_events %>% 
  mutate(begin_date_time = dmy_hms(begin_date_time)
         , end_date_time = dmy_hms(end_date_time)
         , tor_f_scale = as.factor(gsub("^E?F","F",tor_f_scale))
         , event_type = as.factor(event_type)
         , geoid = paste0(
            sprintf("%02d",state_fips) ,sprintf("%03d",cz_fips))
         ) %>%
  transmute(id = event_id, episode_id, begin_date_time, type, end_date_time
         , cz_timezone, geoid, begin_lat, begin_lon, end_lat, end_lon 
         , injuries_direct = as.numeric(injuries_direct)
         , injuries_indirect = as.numeric(injuries_indirect)
         , deaths_direct = as.numeric(deaths_direct)
         , deaths_indirect = as.numeric(deaths_indirect)
         , damage_property, damage_crops
         , tor_f_scale, tor_length, tor_width 
         , event_narrative) %>%
  arrange(begin_date_time) 

