#potential habitat differences from differen tyear types

library(cder)
library(tidyverse)
library(lubridate)
library(wql)

#Use 2011 as probably most simlar to 2023, compare to 2019 (other cages), and 2021 (dry year)

WQ11 = cdec_query(c("GZL", "BDL", "NSL", "RVB", "HUN"), sensors = c(100, 25, 27, 28, 61),
                start.date = as.Date("2011-06-01"), end.date = as.Date("2011-10-31"))
WQ19 = cdec_query(c("GZL", "BDL", "NSL", "RVB", "HUN"), sensors = c(100, 25, 27, 28, 61),
                  start.date = as.Date("2019-06-01"), end.date = as.Date("2019-10-31"))

WQ21 = cdec_query(c("GZL", "BDL", "NSL", "RVB", "HUN"), sensors = c(100, 25, 27, 28, 61),
                  start.date = as.Date("2021-06-01"), end.date = as.Date("2021-10-31"))


#filter
WQ = bind_rows(WQ11, WQ19, WQ21) %>%
  mutate(Year = year(DateTime))

#convert salinity
WQ = mutate(WQ, Value2 = case_when(SensorNumber == 100 ~ ec2pss(Value/1000, 25),
                                   SensorNumber == 25 ~ (Value - 32)*5/9,
                                   TRUE~ Value),
            Analyte = factor(SensorType, levels = c("EL COND", "CHLORPH", "TEMP W", "TURB W",  "DIS OXY"), 
                             labels = c("Salinity", "Chlorophyll", "Temperature", "Turbidity", "DO"))) %>%
  filter(Value2 >0)

ggplot(WQ, aes(x = DateTime, y = Value2, color = StationID)) + 
  facet_wrap(Year~Analyte, scales = "free")+
  geom_line()   + theme_bw()       

#daily averages

WQ2 = mutate(WQ, Day = date(DateTime)) %>%
  group_by( Year, Analyte, Day, StationID) %>%
    summarize(Value = mean(Value2, na.rm =T))

ggplot(WQ2, aes(x = Day, y = Value, color = StationID)) + 
  facet_wrap(Year~Analyte, scales = "free")+
  geom_line()   + theme_bw()  

ggplot(filter(WQ2, StationID %in% c("BDL", "RVB")), aes(x = Day, y = Value, color = StationID)) + 
  facet_wrap(Year~Analyte, scales = "free", nrow = 3)+
  geom_line()   + theme_bw()  
