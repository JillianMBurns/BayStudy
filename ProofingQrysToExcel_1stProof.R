
library(tidyverse)
library(deltadata)
library(openxlsx)
library(geosphere)
library(arsenal)
select <- dplyr::select

currentDate <- Sys.Date()
year <- 2025
survey <- "February"
survey_number <- 02
firstproofer <- "JB"
secondproofer <- "KS"

entryqrys <- bridgeAccess(paste("U:\\LTM\\BayStudy_Lab\\", year, "_Data\\", year, " BS Field Entry.mdb", sep = ""), 
                          c("Station", "Tow", "Distance Outliers", "Bearing Outliers", "Distance High or Low", 
                            "Meters Small or Large","Tows_at_Slack", "Tows_Wrong_Direction", "CableOutDepth", "Meter by date and time"))

## trying to read in first proof to join comments to second proof, will figure out later
# firstproof <- loadWorkbook("U:\\LTM\\BayStudy_Lab\\2024_Data\\ProofingComments\\FirstProof_EntryDB_ November 2024-11-20_KS.xlsx")
# sheetNames <- sheets(firstproof)
# for(i in 1:length(sheetNames))
# {
#   assign(sheetNames[i],readWorkbook(firstproof,sheet = i))
# }

# firstproof_list <- lapply


# rounding to remove ghost numbers
entryqrys$`Distance Outliers` <- entryqrys$`Distance Outliers` %>% 
  mutate(across(6:9, round, 3),
         across(10:12, round, 2))
entryqrys$`Bearing Outliers` <- entryqrys$`Bearing Outliers` %>% 
  mutate(across(5:8, round, 3))
entryqrys$`Distance High or Low` <- entryqrys$`Distance High or Low` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys$`Meters Small or Large` <- entryqrys$`Meters Small or Large` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys$`Tows_at_Slack` <- entryqrys$`Tows_at_Slack` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys$`Tows_Wrong_Direction` <- entryqrys$`Tows_Wrong_Direction` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys$`CableOutDepth` <- entryqrys$`CableOutDepth` %>% 
  mutate(Depth = round(Depth, 2))

# calc distance for 345
tow <- entryqrys$Tow %>% 
  mutate(Time = word(Time, 2), 
         Time = str_sub(Time, 1, nchar(Time)-3)) %>% # grabbing just the time; imported table has unneeded characters in the Time column
  select(Year, Survey, Station, Net, Distance, StartLat, StartLong, EndLat, EndLong) %>% 
  mutate(deg = as.numeric(substr(StartLat, start = 1, stop = 2))) %>% 
  mutate(min = as.numeric(substr(StartLat, start = 3, stop = 4))) %>% 
  mutate(hundMin = as.numeric(substr(StartLat, start = 6, stop = 7))) %>% 
  #mutate(hundMin = as.numeric(case_when(hundMin == 0 ~ 1, TRUE ~ hundMin))) %>%
  mutate(hundMin = as.numeric(ifelse(is.na(hundMin), 0, hundMin))) %>%
  mutate(StartLat_dd = round(deg+((min/60)+(hundMin/6000)), digits = 4)) %>% 
  mutate(deg = as.numeric(substr(EndLat, start = 1, stop = 2))) %>% 
  mutate(min = as.numeric(substr(EndLat, start = 3, stop = 4))) %>% 
  mutate(hundMin = as.numeric(substr(EndLat, start = 6, stop = 7))) %>%
  mutate(hundMin = as.numeric(ifelse(is.na(hundMin), 0, hundMin))) %>%
  #mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(EndLat_dd = round(deg+((min/60)+(hundMin/6000)), digits = 4)) %>% 
  mutate(deg = as.numeric(substr(StartLong, start = 1, stop = 3))) %>% 
  mutate(min = as.numeric(substr(StartLong, start = 4, stop = 5))) %>% 
  mutate(hundMin = as.numeric(substr(StartLong, start = 7, stop = 8))) %>% 
  #mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(hundMin = as.numeric(ifelse(is.na(hundMin), 0, hundMin))) %>%
  mutate(StartLong_dd = -1*(deg+((min/60)+(hundMin/6000)))) %>% 
  mutate(deg = as.numeric(substr(EndLong, start = 1, stop = 3))) %>% 
  mutate(min = as.numeric(substr(EndLong, start = 4, stop = 5))) %>% 
  mutate(hundMin = as.numeric(substr(EndLong, start = 7, stop = 8))) %>% 
  #mutate(hundMin = case_when(hundMin == 0 ~ 1, TRUE ~ hundMin)) %>%
  mutate(hundMin = as.numeric(ifelse(is.na(hundMin), 0, hundMin))) %>%
  mutate(EndLong_dd = -1*(deg+((min/60)+(hundMin/6000)))) %>% 
  select(1:5, StartLat_dd, EndLat_dd, StartLong_dd, EndLong_dd) %>% 
  rename(StartLat = StartLat_dd, StartLong = StartLong_dd, EndLat = EndLat_dd, EndLong = EndLong_dd) %>% 
  mutate(Station = as.character(Station), Survey = as.character(Survey))

# distance calc query in R, works for 345. Add to list
distCalc <- tow %>% 
  filter(Station == 345) %>% 
  rowwise() %>% 
  mutate(distanceCalc = round(((distm(c(StartLong, StartLat), c(EndLong, EndLat), fun = distHaversine))/1852), digits = 2)) %>% 
  mutate(diff = round(abs(distanceCalc - Distance), digits = 2))
entryqrys$DistanceCalc_345 <- distCalc

# add survey column meter by date and time query so you can filter
entryqrys$`Meter by date and time` <- entryqrys$`Meter by date and time` %>% 
  #mutate(Survey = format(Date,"%m", Date = format(Date, "%Y-%m-%d"), Time = format(Time, "%H:%M"))) %>% 
  #filter(Survey == survey_number)%>% 
  mutate(Distance = round(Distance, 2), Tow.Duration = round(Tow.Duration, 2), Depth = round(Depth, 1)) 

#write.xlsx(entryqrys$`Meter by date and time`, "C:\\Users\\JBurns\\Desktop\\MeterByDateandTime.xlsx" )

# add "Other Comments" sheets
OtherComments <- data.frame(Year = "",	Survey = "",	Station = "",	Net = "",	Comment_1stProof = "",	Action_1 = "",	
                            Comment_2ndProof = "",	Action_2 = "", DatabaseEdited = "", DatasheetEdited = "")
entryqrys$OtherComments <- OtherComments

new_columns <- c("Comment_1stProof", "Action_1", "Comment_2ndProof", "Action_2","DatabaseEdited", "DatasheetEdited")
entryqrys_final <- lapply(entryqrys, function(x) {x[new_columns] <- ''; x})

write.xlsx(entryqrys_final, paste("U:\\LTM\\BayStudy_Lab\\2024_Data\\ProofingComments\\FirstProof_EntryDB_", survey, "_",firstproofer,"_", currentDate, ".xlsx", sep = ""))
write.xlsx(entryqrys_final, paste("C:\\Users\\JBurns\\Desktop\\FirstProof_EntryDB_", survey, currentDate, ".xlsx"))
write.xlsx(entryqrys_final, paste("C:\\Users\\JBurns\\Desktop\\SecondProof_EntryDB_", survey, 
                                  "_",currentDate, "_", secondproofer, ".xlsx", sep = ""))

write.xlsx(entryqrys_final, paste("C:\\Users\\JBurns\\Desktop\\EndOfYear_", survey, "_", secondproofer,"_", currentDate, ".xlsx", sep = ""))

# crosstabs
entryqrys_crosstabs <- bridgeAccess(paste("U:\\LTM\\BayStudy_Lab\\", year, "_Data\\", year, " BS Field Entry.mdb", sep = ""),
                                    c("Depth_Crosstab", "Secchi_Crosstab", "Substrate_Crosstab",
                                      "Tide_Bearing_Crosstab"))

columns <- c(2)

entryqrys_crosstabs$Depth_Crosstab <- entryqrys_crosstabs$Depth_Crosstab %>% 
  mutate_all(round, 2) %>% 
  rowwise() %>% 
  mutate(Min = min(c_across(all_of(columns)), na.rm = TRUE), Max = max(c_across(all_of(columns)), na.rm = TRUE), 
         Mean = round(mean(c_across(all_of(columns)), na.rm = TRUE), 2), SD = round(sd(c_across(all_of(columns)), na.rm = TRUE), 2)) %>% 
  mutate(MeanMinusSD = Mean - SD, MeanPlusSD = Mean + SD)
entryqrys_crosstabs$Tide_Bearing_Crosstab <- entryqrys_crosstabs$Tide_Bearing_Crosstab %>% 
  filter(Tide %in% c(1:2))

new_columns <- c("Comment_1stProof", "Action_1", "Comment_2ndProof", "Action_2","DatabaseEdited", "DatasheetEdited")
entryqrys_crosstabs_final <- lapply(entryqrys_crosstabs, function(x) {x[new_columns] <- ''; x})


write.xlsx()
write.xlsx(entryqrys_crosstabs_final, 
           paste("U:\\LTM\\BayStudy_Lab\\", year, "_Data\\ProofingComments\\Crosstabs_", currentDate, ".xlsx", sep = ""))

# add previous years crosstabs - useful when proofing early months

prevyear <- year -1

prevyear_crosstabs <- bridgeAccess(paste("U:\\LTM\\BayStudy_Lab\\", prevyear, "_Data\\", 
                                         prevyear, " BS Field Entry.mdb", sep = ""),
                                   c("Depth_Crosstab", "Secchi_Crosstab", "Substrate_Crosstab",
                                     "Tide_Bearing_Crosstab"))

prevyear_crosstabs$Depth_Crosstab <- prevyear_crosstabs$Depth_Crosstab %>% 
  mutate_all(round, 2) %>% 
  rename_at(vars(-Station), ~ paste0(., "_", prevyear)) 
prevyear_crosstabs$Tide_Bearing_Crosstab <- prevyear_crosstabs$Tide_Bearing_Crosstab %>% 
  filter(Tide %in% c(1:2))%>% 
  rename_at(vars(-Station), ~ paste0(., "_", prevyear))
prevyear_crosstabs$Secchi_Crosstab <- prevyear_crosstabs$Secchi_Crosstab %>% 
  rename_at(vars(-Station), ~ paste0(., "_", prevyear))
prevyear_crosstabs$Substrate_Crosstab <- prevyear_crosstabs$Substrate_Crosstab %>% 
  rename_at(vars(-Station), ~ paste0(., "_", prevyear))

crosstabs_multipleyears_depth <- entryqrys_crosstabs$Depth_Crosstab %>% 
  left_join(prevyear_crosstabs$Depth_Crosstab, by = c("Station"))
crosstabs_multipleyears_secchi <- entryqrys_crosstabs$Secchi_Crosstab %>% 
  left_join(prevyear_crosstabs$Secchi_Crosstab, by = c("Station"))
crosstabs_multipleyears_substrate <- entryqrys_crosstabs$Substrate_Crosstab %>% 
  left_join(prevyear_crosstabs$Substrate_Crosstab, by = c("Station"))
crosstabs_multipleyears_tidebearing <- entryqrys_crosstabs$Tide_Bearing_Crosstab %>% 
  left_join(prevyear_crosstabs$Tide_Bearing, by = c("Station", "Net" = "Net_2024", "Tide" = "Tide_2024"))

crosstabs_multipleyears <- list(Depth_Crosstab = crosstabs_multipleyears_depth, Secchi_Crosstab = crosstabs_multipleyears_secchi, Substrate_Crosstab = crosstabs_multipleyears_substrate,
                                Tide_Bearing_Crosstab = crosstabs_multipleyears_tidebearing)
write.xlsx(crosstabs_multipleyears, 
           paste("U:\\LTM\\BayStudy_Lab\\", year, "_Data\\ProofingComments\\Crosstabs_WithPreviousYears_", currentDate, ".xlsx", sep = ""))



## conditional formatting depth crosstab
wb <- createWorkbook(entryqrys_crosstabs$Depth_Crosstab)
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

## rule applies to all each cell in range\
writeData(wb, "cellIs", -5:5)
#writeData(wb, "cellIs", LETTERS[1:11], startCol = 2)
conditionalFormatting(entryqrys_crosstabs$Depth_Crosstab, "cellIs",
                      cols = 2:12,
                      rows = 2:53, rule = "<Q", style = negStyle
)
conditionalFormatting(wb, "cellIs",
                      cols = 1,
                      rows = 1:11, rule = ">R", style = posStyle
)

#### comparing query results from entry db to sql db

entryqrys_comparetosql <- bridgeAccess("C:\\Users\\JBurns\\Desktop\\2024 BS Field Entry_fortest.mdb", 
                          c("Distance Outliers", "Bearing Outliers", "Distance High or Low", 
                            "Meters Small or Large","Tows_at_Slack"))
# round to remove ghost numbers
entryqrys_comparetosql$`Distance Outliers` <- entryqrys_comparetosql$`Distance Outliers` %>% 
  mutate(across(6:9, round, 3),
         across(10:12, round, 2))
entryqrys_comparetosql$`Bearing Outliers` <- entryqrys_comparetosql$`Bearing Outliers` %>% 
  mutate(across(5:8, round, 3))
entryqrys_comparetosql$`Distance High or Low` <- entryqrys_comparetosql$`Distance High or Low` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys_comparetosql$`Meters Small or Large` <- entryqrys_comparetosql$`Meters Small or Large` %>% 
  mutate(Distance = round(Distance, 2))
entryqrys_comparetosql$`Tows_at_Slack` <- entryqrys_comparetosql$`Tows_at_Slack` %>% 
  mutate(Distance = round(Distance, 2))

sqlqrys <- bridgeAccess("U:\\Public\\BDAH\\SFBayStudy\\PowerApps\\BayStudySQL.accdb",
                        c("Distance Outliers", "Bearing Outliers", "Distance High or Low", "Meters Small or Large",
                          "Tows_at_Slack"))

sqlqrys$`Distance Outliers` <- sqlqrys$`Distance Outliers` %>% 
  mutate(across(6:9, round, 3),
         across(10:12, round, 2))
sqlqrys$`Bearing Outliers` <- sqlqrys$`Bearing Outliers` %>% 
  mutate(across(5:8, round, 3))
sqlqrys$`Distance High or Low` <- sqlqrys$`Distance High or Low` %>% 
  mutate(Distance = round(Distance, 2))
sqlqrys$`Meters Small or Large` <- sqlqrys$`Meters Small or Large` %>% 
  mutate(Distance = round(Distance, 2))
sqlqrys$`Tows_at_Slack` <- sqlqrys$`Tows_at_Slack` %>% 
  mutate(Distance = round(Distance, 2))

## add date time stamp 
#write.xlsx(sqlqrys, "FirstProof_sqlDB.xlsx")

summary(comparedf(entryqrys_comparetosql$`Distance Outliers`, sqlqrys$`Distance Outliers`))
summary(comparedf(entryqrys_comparetosql$`Bearing Outliers`, sqlqrys$`Bearing Outliers`))
summary(comparedf(entryqrys_comparetosql$`Distance High or Low`, sqlqrys$`Distance High or Low`))
summary(comparedf(entryqrys_comparetosql$`Meters Small or Large`, sqlqrys$`Meters Small or Large`))
summary(comparedf(entryqrys_comparetosql$`Tows_at_Slack`, sqlqrys$`Tows_at_Slack`))

#distance clc in  R - does it work for 345

## testing calc distance
library(geosphere)
StartLat <- 37.7595
StartLong <- -122.4337
EndLat <- 37.8595
EndLong <- -122.555
 

distanceCalc_natuicalmiles <- round(((distm(c(StartLong, StartLat), c(EndLong, EndLat), fun = distHaversine))/1852), digits = 2)
distanceCalc_n <- round(((distm(c(StartLong, StartLat), c(EndLong, EndLat), fun = distHaversine))), digits = 2)


# testing bearing
StartLat <- 37.7595
StartLong <- -122.4337
EndLat <- 38.8595
EndLong <- -122.555

p1 <- c(122.4337, 37.7595)
p2 <- c(122.555, 38.8595)

bearing(p1, p2)
