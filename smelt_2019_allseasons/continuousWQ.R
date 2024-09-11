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
             color = "black")+
  scale_linetype_manual(values = c(1,3,5))

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
            MinValue = min(Value2, na.rm =T)) %>%
  mutate(Location = case_when(StationID == "BDL" ~ "SM",
                              StationID == "LIS" ~ "Yolo", 
                              StationID == "RVB" ~ "RV", 
                              StationID == "DWS" ~ "DWSC"),
         Location = factor(Location, levels = c("SM", "DWSC", "Yolo", "RV")))

ggplot(WQmean, aes(x = Date, y = MeanValue,
                   color = StationID))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  geom_ribbon(aes(ymin = MinValue, ymax = MaxValue, fill = StationID, color = NULL), alpha = 0.4)+
  ylab("Turbidity (FNU)          Temperature (C)             Salinity (PSU)         Dissolved Oxygen (mg/L)")+
  xlab("Date")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  scale_color_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  scale_fill_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "black")+
  scale_linetype_manual(values = c(1,3,5))


ggsave("plots/ContWaterQuality.tiff", device = "tiff", width =8, height =7)

#Summary statistics
library(wql)
cagechecks2019 <- read_csv("smelt_2019_allseasons/data_raw/Field_Environmental_Data_Oct_qaqc'ed_12.11.20.csv")%>%
  mutate(Salinity = ec2pss(SpCond_uS/1000, t = 25), Date = mdy(Date), Season = "Fall") %>%
  rename(`Dissolved Oxygen` = DO_mg_L, Temperature = Water.Temp_C, Turbidity = Avg.Turb_ntu) %>%
  select(Location, Date, Season, `Dissolved Oxygen`, Salinity, Temperature, Turbidity)
cagechecks20192 <- read_csv("smelt_2019_allseasons/data_raw/Field_Environmental_Data_Aug_qaqc'ed_11.04.20.csv")%>%
  mutate(Salinity = ec2pss(SpCond_uS/1000, t = 25), Date = mdy(Date), Season = "Summer") %>%
  rename(`Dissolved Oxygen` = DO_mg_L, Temperature = Water.Temp_C, Turbidity = Avg.Turb_ntu)%>%
  select(Location, Date, Season, `Dissolved Oxygen`, Salinity, Temperature, Turbidity)
cagechecks20193 <- read_csv("smelt_2019_winterspring/data_raw/2019_smeltstudy_dailycheck_data.csv")%>%
  mutate(Salinity = ec2pss(SpCond/1000, t = 25), Date = mdy(Date), Season = "Winter") %>%
  rename(`Dissolved Oxygen` = DO, Temperature = WaterTemp, Turbidity = AvgTurb)%>%
  select(Location, Date, Season, `Dissolved Oxygen`, Salinity, Temperature, Turbidity)

checks = bind_rows(cagechecks2019, cagechecks20192, cagechecks20193)


checkslong = checks%>%
  pivot_longer(cols = c(Salinity, Temperature, Turbidity, `Dissolved Oxygen`),
                        names_to = "Analyte", values_to = "Value")

wq_wdiscrete = left_join(WQmean, checkslong)    %>%
  mutate(Season = factor(Season, levels = c("Winter", "Summer", "Fall"))) %>%
  mutate( Location = factor(Location, levels = c("SM", "DWSC", "Yolo", "RV")))

ggplot(wq_wdiscrete, aes(x = Date, y = MeanValue,
                   color = Location))+ 
  facet_grid(Analyte~Season, scales = "free")+
  geom_line()+
  geom_ribbon(aes(ymin = MinValue, ymax = MaxValue, fill = Location, color = NULL), alpha = 0.4)+
  geom_point(aes(y = Value))+
  ylab("Turbidity (FNU)          Temperature (C)             Salinity (PSU)         Dissolved Oxygen (mg/L)")+
  xlab("Date")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  scale_color_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  scale_fill_brewer(palette = "Dark2", labels = c("Suisun Marsh", "SDWSC", "Yolo", "Rio Vista"), name = "Location")+
  geom_hline(data = cuttoffs, aes(yintercept = cuttoff, linetype = Type), 
             color = "black")+
  scale_linetype_manual(values = c(1,3,5), name ="Smelt Tolerance")


ggsave("plots/ContWaterQuality_wdiscrete.tiff", device = "tiff", width =8, height =7)
