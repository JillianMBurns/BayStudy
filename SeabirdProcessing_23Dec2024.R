library(oce)
library(ocedata)
library(tidyverse)
library(lubridate)
library(patchwork)
library(openxlsx)

#

# load data ---------------------------------------------------------------

## ID month and year you are working with
month <- "February"
survey <- 2
year <- 2025
castnumber <- 52
currentDate <- Sys.Date()

## determine path
pathname <- paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, "\\", month, sep = "")

## get file names
files<- list.files(path = pathname, pattern = "*.cnv", full.names=TRUE)

##get all the .cnv file from the directory
# 52 is the number of files in the folder; need to change if different number of casts during the survey
  
  ctd = list()

for (i in 1:castnumber){
  
  ctd[[i]] = read.ctd(files[i])%>%
    ctdTrim(method = "downcast")
    #%>%    ctdDecimate(p = 0.2)
}
  ## name casts; need to know how many casts, change as needed
  names(ctd) <- paste("raw_", 1:castnumber, sep = "")


# get data from list ------------------------------------------------------

  
getdata <-  lapply(ctd, slot, name = "data")
profiles <- purrr::map_df(getdata, data.frame, .id = 'name')  %>% 
  # filter(scan > 100) %>% 
  #filter(descentRate > 0.01) %>% 
  mutate(survey = survey, year = year)
 

# get start time from metadata to help match station to cast -------------

## extracting time
ctd$raw_1[[1]]
  a <- ctd %>% 
    map(slot, name = "metadata")
   b <- lapply(a, function (x) `[`(x, c('startTime'))) 
   df <- data.frame(matrix(unlist(b), nrow=52, byrow=TRUE),stringsAsFactors=FALSE) #change cast number manually
   df <- as.POSIXct(df$matrix.unlist.b...nrow...52..byrow...TRUE., origin ="1970-01-01 00:00:00", tz = "UTC") #change cast number manually
   df <- as.data.frame(df)

## make dataframe
starttimes <- df %>% 
  mutate(name = paste("raw_", 1:castnumber, sep = "")) %>% 
  rename(startTime = df) %>% 
  select(name, startTime) %>% 
  mutate(startTime = as.character(startTime)) %>% 
  mutate(station = "", replicate = "")

## save csv to match station using seabird log, when matched re-save with same name
path2 <- paste(pathname, "\\StartTimesForMatch_", month, year, ".csv", sep = "")
write_csv(starttimes, file = path2)

## read in matched cast/station

matched <- read_csv(path2) %>% 
  select(name, station, replicate)


## add station to profile data
## LEFT OFF HERE TRYING TO DO ROUNDED DEPTH AND DEPTH BIN:
### I THINK I NEED TO REMOVE NEGATIVE DESCENT RATE
rawprofiles <- profiles %>% 
  left_join(matched, by = "name") %>% 
  filter(replicate ==1) %>% 
  select(-replicate) %>% 
  filter(station < 900) %>% 
  relocate(c(year, survey, station)) 
  


path3 <- paste(pathname, "\\rawProfiles_", month, year, ".csv", sep = "") 
write_csv(rawprofiles, path3)
write.xlsx(rawprofiles, paste(pathname, "\\rawProfiles_", month, year, ".xlsx", sep = ""))



#---------------------------------Combining all months

seabirdfiles <- list.files(path = (paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, sep = "")), recursive = T, pattern = "rawProfiles.+csv", full.names = T)

print(list.files(path = (paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, sep = "")), recursive = T, pattern = "rawProfiles.+csv"))

data_list <- list()

for (file in seabirdfiles) {
  data <- read.csv(file)
  data_list[[file]] <- data 
}

seabird_rawcombined <- bind_rows(data_list) #%>% 
  #select(-roundedDepth, -depthCode) # decided to do depth code after combining. 

  # save raw combined data
write_csv(seabird_rawcombined, 
          file = paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, "\\RawCombined_",year, "_", currentDate, ".csv", sep = ""))

## scrub data and add depth codes
seabird_scrubbed <- seabird_rawcombined %>% 
  filter(descentRate > 0.05 & scan > 100) %>% 
  mutate(roundedDepth = cut(depth, breaks = c(0.0, 0.2, 0.7, 1.2, 1.7, 2.2, 2.7, 3.2, 3.7, 4.2, 4.7, 5.2, 5.7, 6.2, 6.7,
                                              7.2, 7.7, 8.2, 8.7, 9.2, 9.7, 10.2, 10.7, 11.2, 11.7, 12.2, 12.7,
                                              13.2, 13.7, 14.2, 14.7, 15.2, 15.7, 16.2, 16.7, 17.2, 17.7, 18.2, 18.7, 
                                              19.2, 19.7, 20.2, 20.7, 21.2, 21.7, 22.2, 22.7, 23.2, 23.7, 24.2, 24.7, 
                                              25.2, 25.7, 26.2, 26.7, 27.2, 27.7, 28.2, 28.7, 29.2, 29.7, 30.2, 30.7, 31.2),
                            # labels = c("0","0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4.0", "4.5", "5.0", "5.5", "6.0",
                            #            "6.5", "7.0", "7.5", "8.0", "8.5", "9.0", "9.5", "10.0", "10.5",
                            #            "11.0", "11.5", "12.0", "12.5", "13.0", "13.5", "14.0", "14.5", "15.0",
                            #            "15.5", "16.0", "16.5", "17.0", "17.5", "18.0", "18.5", "19.0", "19.5",
                            #            "20.0", "20.5", "21.0", "21.5", "22.0", "22.5", "23.0", "23.5", "24.0",
                            #            "24.5", "25.0", "25.5", "26.0"))) %>% 
                            labels = c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0,
                                       6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0, 10.5,
                                       11.0, 11.5, 12.0, 12.5, 13.0, 13.5, 14.0, 14.5, 15.0,
                                       15.5, 16.0, 16.5, 17.0, 17.5, 18.0, 18.5, 19.0, 19.5,
                                       20.0, 20.5, 21.0, 21.5, 22.0, 22.5, 23.0, 23.5, 24.0,
                                       24.5, 25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0, 28.5, 29.0, 
                                       29.5, 30.0, 30.5, 31.0))) %>% 
  mutate(roundedDepth = as.character(roundedDepth)) %>% 
  group_by(year, survey, station) %>%
  mutate(depthCode = ifelse(roundedDepth == max(as.numeric(roundedDepth), na.rm = T), 3, 2)) %>%
  mutate(depthCode = ifelse(depth < 1.2, 1, depthCode))

# save scrubbed data
# save raw combined data
write_csv(seabird_scrubbed, 
          file = paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, "\\Scrubbed_",year, "_", currentDate, ".csv", sep = ""))
# write.xlsx(seabird_scrubbed, 
#           file = paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, "\\Scrubbed_",year, "_", currentDate, ".xlsx", sep = ""))


#------------------------------review 

seabird_scrubbed <- read_csv("U:\\LTM\\Bay Study\\SeaBird\\Data\\2024\\Scrubbed_2024_2024-12-23.csv")

manualcolors<-c('forestgreen', 'red2', 'orange', 'cornflowerblue', 
                'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue', 
                'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', 'tan3',
                "tan1",'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 
                'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                "darkolivegreen1" ,"tan2" ,   "tomato3" , "#7CE3D8","gainsboro")

binned <- seabird_scrubbed %>% 
  group_by(year, survey, station, roundedDepth, depthCode) %>% 
  summarise(meanTemp = mean(temperature, na.rm = T), meanSalin = mean(salinity, na.rm = T))
  
names(manualcolors) <- levels(factor(c(levels(binned))))

# make nested table in order to make a plot per station
nested <- binned %>% 
  mutate(station = as.factor(station)) %>% 
  group_by(station) %>% 
  nest()

#salinity raw data
bs_seabird_salin_plots_bin <- nested %>% 
  mutate(plot = map2(station, data, function(.x, .y) { 
    ggplot(data = .y, aes(x = meanSalin, y = roundedDepth, shape = as.factor(depthCode), color = as.factor(survey)  
                          #label = DepthCode, 
                          #shape = as.factor(depthCode)
    )) +
      geom_point() +
      #geom_text() +
      #geom_line(size = 1.25, alpha = .6) +
      geom_hline(yintercept = 1) +
      scale_color_manual(values = manualcolors) +
      #scale_color_manual(values = c("red","blue")) +
      theme_classic()  +
      scale_y_reverse() +
      #scale_x_continuous(breaks = round(seq(min(sonde_raw$Salinity), max(sonde_raw$Salinity), by = 1),1)) +
      ggtitle(label = .x)
  } ))

# plots will appear in a PDF in the working directory
pdf(paste("U:\\LTM\\Bay Study\\SeaBird\\Data\\", year, "\\", year, "Salinity_bin_", currentDate, ".pdf", sep=""))
bs_seabird_salin_plots_bin$plot
dev.off()

# review individual plots
seabird_scrubbed %>% 
  group_by(year, survey, station, roundedDepth, depthCode) %>% 
  summarise(meanTemp = mean(temperature, na.rm = T), meanSalin = mean(salinity, na.rm = T)) %>% 
  filter(station == 140) %>% 
  ggplot(aes(x=meanSalin, y=roundedDepth, shape = as.factor(depthCode), color = as.factor(survey))) +
  scale_color_manual(values = surveycolors) +
  geom_point() +
  scale_y_reverse() +
  theme_bw()
seabird_scrubbed %>% 
  # filter(descentRate > 0.05) %>% 
  # filter(depth > 0.25) %>% 
  # filter(scan >100) %>% 
  filter(station == 140) %>% 
  ggplot(aes(x=salinity, y=depth, color = as.factor(depthCode))) +
  geom_point() +
  scale_y_reverse()
rawprofiles %>% 
  filter(descentRate > 0.05) %>% 
  filter(depth > 0.25) %>% 
  filter(station == 101) %>% 
  ggplot(aes(x=salinity, y=depth, color = as.factor(station))) +
  geom_point(color = "blue") +
  scale_y_reverse()

seabird_rounded <- seabird_scrubbed %>% 
  group_by(year, survey, station, roundedDepth, depthCode) %>% 
  summarise(meanSalinity = mean(salinity, na.rm = T), meanTemp = mean(temperature, na.rm = T))

# test rounded depth
seabird_rounded %>% 
  filter(station == 102) %>% 
  ggplot(aes(x=meanSalinity, y=roundedDepth, color = as.factor(survey))) +
  geom_point() +
  scale_y_reverse()
  

## plot

cleanprofiles %>% 
  filter(station == 101) %>% 
  #filter(descentRate > 0.01) %>% 
  ggplot(aes(x=salinity, y=depth, color = as.factor(station))) +
  geom_point() +
  scale_y_reverse()

# plotting station 101 front

st101_temp <- cleanprofiles %>% 
  filter(descentRate >.15) %>% 
  ggplot(aes(x=temperature, y=depth, color = as.factor(stationlabel))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("cyan2", "burlywood4")) +
  scale_y_reverse() +
  labs(color = "water clarity") +
  theme_classic()

st101_salin <- cleanprofiles %>% 
  filter(descentRate >.15) %>% 
  ggplot(aes(x=salinity, y=depth, color = as.factor(stationlabel))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("cyan2", "burlywood4")) +
  scale_y_reverse() +
  labs(color = "water clarity") +
  theme_classic()

st101_temp / st101_salin +
  plot_layout(guides = 'collect')

# Archive -----------------------------------------------------------------

## Another way to load profiles

files<- dir("U:\\LTM\\Bay Study\\SeaBird\\Data\\2022\\October",pattern = "*.cnv") #get all the .cnv file from the directory

casts<- list() #make a list dataframe
for (ifile in 1:length(files)) {
  casts[[ifile]]<- read.ctd.sbe(files[ifile]) 
}
casts <- read.oce(dir("U:\\LTM\\Bay Study\\SeaBird\\Data\\2022\\October",pattern = "*.cnv"))


filenames <- list.files(path = "U:\\LTM\\Bay Study\\SeaBird\\Data\\2022\\October", pattern="*.cnv", full.names=TRUE)
casts <- lapply(filenames, read.ctd.sbe)
names(casts) <- paste("raw_", 1:53, sep = "")

casts$raw_1@data%>%as.data.frame()%>%head()
summary(casts$raw_1)

plot(casts$raw_1)

casts_trim <- casts %>% 
  lapply(ctdTrim)

attributes(casts_trim)

casts_tables <- lapply(casts_trim$, as.tibble)

plot(casts_clean$raw_1)
