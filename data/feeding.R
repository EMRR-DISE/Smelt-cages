setwd("C:/Users/smeyer/California Department of Water Resources/DWR Documents - Aquatic Ecology Section (AES) - Documents/Delta Smelt Enclosure Study/Feeding Experiment (2022)/Data/Ambient Zooplankton")

library(tidyverse)
library(dplyr)
library(ggpubr)
library(scales)

crosswalk <- read.csv("Cage Biomass crosswalk Updated.csv")
#Read in and clean up datasets
#Seperate sampling dates were saved in individual csv files, so I compiled them
zoop1 <- read_csv("RIO VISTA_2022-02-08_150_SMELT STUDY.csv")
zoop2 <- read_csv("RIO VISTA_2022-02-10_150_SMELT STUDY.csv")
zoop3 <- read_csv("RIO VISTA_2022-02-16_150_SMELT STUDY.csv")
zoop4 <- read_csv("RIO VISTA_2022-02-22_150_SMELT STUDY.csv")
zoop5 <- read_csv("RIO VISTA_2022-03-01_150_SMELT STUDY.csv")
zoop6 <- read_csv("RIO VISTA_2022-03-09_150_SMELT STUDY.csv")
all_zoop <- rbind(zoop1, zoop2, zoop3, zoop4, zoop5, zoop6)

#2022 EMP monitoring data was pulled from the big spreadsheet of all time EMP data for Meso and Pump samples
monitoring_meso <- readxl::read_xlsx("./monitoring_data/1972-2023_CBMatrix_Public.xlsx", sheet = "1972-2023_CB_Matrix") %>%
  filter(., StationNZ == "NZ064", Year == 2022) #Just station 64 and year 2022
monitoring_meso <- monitoring_meso[,-grep("ALL", colnames(monitoring_meso), fixed=T)] #Removes the All... columns because they are sums of other data

monitoring_pump <- readxl::read_xlsx("./monitoring_data/1972-2023_PumpMatrix_Public.xlsx", sheet = "1980-2023_Pump_CPUE_Matrix") %>%
  filter(., StationNZ == "NZ064", Year == 2022) 
monitoring_pump <- monitoring_pump[,-grep("ALL", colnames(monitoring_pump), fixed=T)]

#Crosswalking with crosswalk table
feeding2022 <- left_join(all_zoop, select(crosswalk, FeedingCode2022, TaxaGroup), by = c("taxon" = "FeedingCode2022") )

station2022_meso <- monitoring_meso %>%
  gather(., key = 'Prey', value = 'CPUE', ACARTELA:CRABZOEA) %>%
  left_join(., select(crosswalk, EMP_Code, TaxaGroup), by = c("Prey" = "EMP_Code"))

station2022_pump <- monitoring_pump %>%
  gather(., key = 'Prey', value = 'CPUE', LIMNOSINE:BARNNAUP) %>%
  left_join(., select(crosswalk, EMP_Code, TaxaGroup), by = c("Prey" = "EMP_Code"))

#set plot colors
palette1 = scales::hue_pal()(length(unique(feeding2022$taxon)))
palette1_named = setNames(object = palette1, nm = sort(unique(feeding2022$taxon)))

#Plots for "TaxaGroup" meaning taxonomy grouped into the largest resolution
ggplot(feeding2022, aes(x = date, y = count, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

ggplot(station2022_pump, aes(x = SampleDate, y = CPUE, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

ggplot(station2022_meso, aes(x = SampleDate, y = CPUE, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

#Plots by the highest taxonomic resolution we have for each dataset
ggplot(feeding2022, aes(x = date, y = count, fill = taxon)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

ggplot(station2022_pump, aes(x = SampleDate, y = CPUE, fill = Prey)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

ggplot(station2022_meso, aes(x = SampleDate, y = CPUE, fill = Prey)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 Feeding Study - Rio Vista")

#Plots by the highest taxonomic resolution we have for each dataset reduced to date range
fed <- ggplot(feeding2022, aes(x = as.factor(date), y = count, fill = taxon)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  #scale_fill_manual(values = palette1_named) +
  labs(title = "2022 Feeding Study - Rio Vista", x="Date")
  

mon_pump <- ggplot(filter(station2022_pump, Survey == 2 | Survey == 3, TaxaGroup != "TotalLimno"), aes(x = as.factor(SampleDate), y = CPUE, fill = Prey)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 EMP Feb and Mar Pump Collection", x="Date") 

mon_meso <- ggplot(filter(station2022_meso, Survey == 2 | Survey == 3),  aes(x = as.factor(SampleDate), y = CPUE, fill = Prey)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 EMP Feb and Mar Meso Collection", x="Date")  

ggarrange(fed,                                                 # First row with scatter plot
          ggarrange(mon_pump, mon_meso, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 


#set plot colors
palette2 = hue_pal()(length(unique(feeding2022$TaxaGroup)))
palette2_named = setNames(object = palette2, nm = sort(unique(feeding2022$TaxaGroup)))

fed <- ggplot(feeding2022, aes(x = as.factor(date), y = count, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  scale_fill_manual(values = palette2_named) +
  labs(title = "2022 Feeding Study - Rio Vista", x="Date")
  

mon_pump <- ggplot(filter(station2022_pump, Survey == 2 | Survey == 3), aes(x = as.factor(SampleDate), y = CPUE, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 EMP Feb and Mar Pump Collection", x="Date") +
  scale_fill_manual(values = palette2_named)

mon_meso <- ggplot(filter(station2022_meso, Survey == 2 | Survey == 3),  aes(x = as.factor(SampleDate), y = CPUE, fill = TaxaGroup)) + 
  geom_col(position = "identity") +
  #geom_col(position = "fill") +
  labs(title = "2022 EMP Feb and Mar Meso Collection", x="Date") +
  scale_fill_manual(values = palette2_named)

ggarrange(fed,                                                 # First row with scatter plot
          ggarrange(mon_pump, mon_meso, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 


