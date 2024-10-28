#look at water quality comparing all the Suisun/RIo Vista deployments

library(tidyverse)
library(cder)
library(wql)

#pull data from CDEC for Beldens and rio vista from 2019-2024
WQcages = cdec_query(stations = c("BDL", "RVB"), sensors = c(25, 27, 100), durations = "E",
           start.date = ymd("2019-01-01"), end.date = ymd("2024-10-10"))

#set up a data frame with cage deployment dates
Cagedates = data.frame(Year = c(2019, 2023, 2024),
                       StartDate = c(ymd("2019-10-09"), ymd("2023-08-30"), ymd("2024-08-27")),
                       EndDate = c(ymd("2019-11-07"), ymd("2023-10-10"), ymd("2024-09-26")))

#create a date, Day-of-year, and year variable
WQcages = mutate(WQcages, Day = date(ObsDate), DOY = yday(ObsDate), Year = year(ObsDate))

#filter water quality to just the deployment dates
WQcages2 = left_join(WQcages, Cagedates) %>%
  filter(Day > StartDate & Day < EndDate)

#plot it
ggplot(WQcages2, aes(x = DOY, y = Value, color =StationID)) +
  geom_line()+
  facet_grid(SensorType ~ Year, scales = "free_y")

ggplot(WQcages2, aes(x = as.factor(Year), y = Value, color =StationID)) +
  geom_boxplot()+
  facet_grid(SensorType ~ ., scales = "free_y")

#create daily averages
WQsum = group_by(WQcages2, Day, DOY, StationID, Year, SensorType) %>%
  summarise(MeanValue = mean(Value, na.rm =T)) %>%
  #convert F to C
  mutate(MeanValue2 = case_when(SensorType == "TEMP W" ~ (MeanValue -32)*5/9, 
                                #convert EC to salinity
                                SensorType == "EL COND" ~ ec2pss(MeanValue/1000, 20),
                                TRUE ~ MeanValue))

#plot it
ggplot(WQsum, aes(x = DOY, y =MeanValue, color =StationID)) +
  geom_line()+
  facet_grid(SensorType ~ Year, scales = "free_y")

#create means for the entire time period.
WQmeans = group_by(WQcages2, Year, StationID, SensorType) %>%
  summarise(MeanValue = mean(Value, na.rm =T)) %>%
  mutate(MeanValue2 = case_when(SensorType == "TEMP W" ~ (MeanValue -32)*5/9,
                                SensorType == "EL COND" ~ ec2pss(MeanValue/1000, 20),
                                TRUE ~ MeanValue))
  
         