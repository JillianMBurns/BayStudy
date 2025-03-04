library(tidyverse)
library(deltadata)
library(readxl)

# assign year to get correct file path
year <- 2023

# file path for lab entry file on u-drive
folder <- paste("U:\\LTM\\BayStudy_Lab\\", year, "_Data\\", sep = "")
file <- paste(year, "BS Field Entry.mdb", sep = " ")
fieldentry_path <- paste(folder, file, sep = "") # path for field entry file

# the bridgeAccess function actually gets the tables
stationtow <- bridgeAccess(fieldentry_path,
                           c("Tow")) # specify tables you want from the database

# prepare data for tow plotting function (from deltadata, created by Trinh)
# the function is described here: https://github.com/trinhxuann/deltadata

# clean tow table; mainly converting coordinates to decimal degrees
tow <- stationtow$Tow %>% 
  mutate(Time = word(Time, 2), 
         Time = str_sub(Time, 1, nchar(Time)-3)) %>% # grabbing just the time; imported table has unneeded characters in the Time column
  select(Year, Survey, Station, Net, StartLat, StartLong, EndLat, EndLong) %>% 
  mutate(deg = as.numeric(substr(StartLat, start = 1, stop = 2))) %>% 
  mutate(min = as.numeric(substr(StartLat, start = 3, stop = 4))) %>% 
  mutate(hundMin = as.numeric(substr(StartLat, start = 6, stop = 7))) %>% 
  mutate(hundMin = as.numeric(case_when(hundMin == 0 ~ 1, TRUE ~ hundMin))) %>%
  mutate(StartLat_dd = round(deg+((min/60)+(hundMin/6000)), digits = 4)) %>% 
  mutate(deg = as.numeric(substr(EndLat, start = 1, stop = 2))) %>% 
  mutate(min = as.numeric(substr(EndLat, start = 3, stop = 4))) %>% 
  mutate(hundMin = as.numeric(substr(EndLat, start = 6, stop = 7))) %>% 
  mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(EndLat_dd = round(deg+((min/60)+(hundMin/6000)), digits = 4)) %>% 
  mutate(deg = as.numeric(substr(StartLong, start = 1, stop = 3))) %>% 
  mutate(min = as.numeric(substr(StartLong, start = 4, stop = 5))) %>% 
  mutate(hundMin = as.numeric(substr(StartLong, start = 7, stop = 8))) %>% 
  mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(StartLong_dd = -1*(deg+((min/60)+(hundMin/6000)))) %>% 
  mutate(deg = as.numeric(substr(EndLong, start = 1, stop = 3))) %>% 
  mutate(min = as.numeric(substr(EndLong, start = 4, stop = 5))) %>% 
  mutate(hundMin = as.numeric(substr(EndLong, start = 7, stop = 8))) %>% 
  mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(EndLong_dd = -1*(deg+((min/60)+(hundMin/6000)))) %>% 
  select(1:4, StartLat_dd, EndLat_dd, StartLong_dd, EndLong_dd) %>% 
  rename(StartLat = StartLat_dd, StartLong = StartLong_dd, EndLat = EndLat_dd, EndLong = EndLong_dd) %>% 
  mutate(Station = as.character(Station), Survey = as.character(Survey))

# get tow data in correct format; separated starts and ends, then joined back together
tow_starts <- tow %>% 
  select(1:4, StartLat, StartLong) %>% 
  mutate(legend = "Start") %>% 
  rename(lat = StartLat, lon = StartLong) %>% 
  relocate(legend, .after = "Net")
tow_ends <- tow %>% 
  select(1:4, EndLat, EndLong) %>% 
  mutate(legend = "End") %>% 
  rename(lat = EndLat, lon = EndLong) %>% 
  relocate(legend, .after = "Net")
tow_coords <- bind_rows(tow_starts, tow_ends) %>% 
  rename(date = Year, station = Station, layer = Survey)

# clean theoretical station coorindates
stations_theoretical <- read_excel("U:\\LTM\\Bay Study\\SFBS_EDI\\Databases\\Refdata\\Bay Study_Station Coordinates for Distribution_04May2020.xlsx", skip=2)%>% #excel file with theoretical station coordinates
  filter(!is.na(Longitude)) %>% #removes text rows from original excel file
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep ="°", convert=T)%>% #separating cooridnates in order to convert to decimal degrees
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(lat=round((Lat_Deg+Lat_Min/60), digits = 4), #converting to decimal degrees
         lon=Lon_Deg-Lon_Min/60)%>%
  select(Station, lat, lon)%>%
  filter(Station!="211E")%>% # Kathy said W location of station 211 is more often used, so using those coordinates
  mutate(Station=recode(Station, `211W`="211")) %>% 
  mutate(legend = "Theoretical", layer = "Theoretical") %>% 
  relocate(legend, .after = "Station") %>% 
  relocate(layer, .after = "legend") %>% 
  rename(station = Station)
# combine theoretical and actual tow coordinates
gpsData <- bind_rows(tow_coords, stations_theoretical) %>% 
  mutate(Net = ifelse(is.na(Net), 999, Net)) %>% 
  filter(!Net == 2) %>% # filter net here for Bay Study, only look at one at a time. The filter must remove the net you do not want to look at so the theoretical coordinates do not get filtered out
  select(-Net) #function requires a df with 6 specific columns

# remove NAs
gpsData <- subset(gpsData, !is.na(lat) | !is.na(lon))

# plot data
## Turn on and off surveys via the check boxes in the map depending what survey you are reviewing
plotGPS(gpsData, layerName = "Survey", dateName = "Year", height = 500)

# get outliers
## mainly looking for egregious outliers

# makes table of outliers; you could save this a keep track of outliers
gpsOutliersData <- gpsOutlier(gpsData, d = 1)

# plots outliers
plotGPS(gpsOutliersData, height = 500)

