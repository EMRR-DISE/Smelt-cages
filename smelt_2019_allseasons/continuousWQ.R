#Continuous water quality

library(lubridate)
library(tidyverse)
library(cder)
library(wql)
library(readxl)

WQ = cdec_query(stations = c("LIS", "BDL", "RVB", "DWS"), 
                sensors = c(24,25, 100, 27, 221, 61), 
                start.date = ymd("2019-01-01"), 
                end.date = ymd("2020-01-01"))


WQt = filter(WQ, Value >0) %>%
  mutate(Value2 = case_when(SensorType == "EL COND" ~  ec2pss(Value/1000, 25),
                            SensorType == "TEMP W" ~ (Value-32)*5/9,
                            TRUE ~ Value),
         Analyte = case_when(SensorType == "EL COND"~ "Salinity",
                             SensorType == "TURB W" ~ "Turbidity",
                             SensorType == "TURB WF" ~ "Turbidity",
                           SensorType == "TEMP W"~ "Temperature",
                           SensorType == "DIS OXY" ~ "Dissolved Oxygen")) %>%
  filter(Value2 <1000, Value2>0,  Duration == "E")


ggplot(WQt, aes(x = ObsDate, y = Value2,
               color = StationID))+ 
  facet_wrap(~Analyte, scales = "free_y")+
  geom_line()
#it still looks like salinity drops to zero for no reason

group_by(WQt, Analyte) %>%
  summarize(mean = mean(Value2), min = min(Value2), max = max(Value2))

dates = read_excel("data/2019 Cage Deployment Dates.xlsx") %>%
  mutate(Deployment = paste(StationID, Season))

#Attach the deployment dates and filter to just periods with deployments
WQ3 = left_join(WQt, dates) %>%
  mutate(deploy = case_when(ObsDate >= Start & ObsDate <= End ~ TRUE,
                          TRUE  ~ FALSE),
         Season = factor(Season, levels = c("Winter", "Summer", "Fall"))) %>%
  filter(deploy)

ggplot(WQ3, aes(x = ObsDate, y = Value2,
                color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  ylab("Value")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")

#Add the cuttoffs for good delta smelt habitat

cuttoffs = data.frame(Analyte = c("Dissolved Oxygen", "Salinity", "Temperature","Temperature", "Turbidity"),
                      Type = c("Min", "Max", "Suitable","Max", "Min"),
             cuttoff = c(5, 6, 22, 25, 12))

ggplot(WQ3, aes(x = ObsDate, y = Value2,
                color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  ylab("Value")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "red")

#now the mean with rectangles

WQmean = mutate(WQ3, Date = date(ObsDate)) %>%
  group_by( StationID, Season, Analyte, Date)%>%
  summarize(MeanValue = mean(Value2, na.rm =T), MaxValue = max(Value2, na.rm =T),
            MinValue = min(Value2, na.rm =T))

ggplot(WQmean, aes(x = Date, y = MeanValue,
                color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  geom_ribbon(aes(ymin = MinValue, ymax = MaxValue, fill = StationID, color = NULL), alpha = 0.4)+
  ylab("Value")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "red")

###################################################################
#Try again with all stations on all dates


#Attach the deployment dates and filter to just periods with deployments
WQ3 = cross_join(WQt, select(dates, -StationID)) %>%
  mutate(deploy = case_when(ObsDate >= Start & ObsDate <= End ~ TRUE,
                            TRUE  ~ FALSE),
         Season = factor(Season, levels = c("Winter", "Summer", "Fall"))) %>%
  filter(deploy)


ggplot(WQ3, aes(x = ObsDate, y = Value2,
                color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  ylab("Value")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")

#Add the cuttoffs for good delta smelt habitat

cuttoffs = data.frame(Analyte = c("Salinity", "Temperature","Temperature", "Turbidity"),
                      Type = c( "Max", "Suitable","Max", "Min"),
                      cuttoff = c(6, 22, 25, 12))

ggplot(WQ3, aes(x = ObsDate, y = Value2,
                color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  ylab("Value")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "red")

#now the mean with rectangles

WQmean = mutate(WQ3, Date = date(ObsDate)) %>%
  group_by( StationID, Season, Analyte, Date)%>%
  summarize(MeanValue = mean(Value2, na.rm =T), MaxValue = max(Value2, na.rm =T),
            MinValue = min(Value2, na.rm =T))

ggplot(WQmean, aes(x = Date, y = MeanValue,
                   color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  geom_ribbon(aes(ymin = MinValue, ymax = MaxValue, fill = StationID, color = NULL), alpha = 0.4)+
  ylab("Turbidity (FNU)          Temperature (C)             Salinity (PSU)         Dissolved Oxygen (mg/L)")+
  xlab("Date")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  scale_fill_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "red")

ggsave("plots/ContWaterQuality.tiff", device = "tiff", width =8, height =7)
