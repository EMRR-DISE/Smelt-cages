#compare all three years

library(tidyverse)
library(readxl)

cf2019 = read_csv("smelt_2019_allseasons/data_clean/clean_smelt_cf_2019.csv")
cf2019f = filter(cf2019, Deployment == "Fall", Site %in% c("RV", "SM")) %>%
  rename(Weight = Weight_g, Location = Site, CageNumber = Cage, FishID = ID) %>%
  mutate(ForkLength = FL_cm*10, Condition = 100*(Weight/(ForkLength*0.1)^3))%>%
  select(Location, CageNumber, FishID, ForkLength, Weight, Condition) %>%
  mutate(Year = 2019, Location = case_when(Location == "RV" ~ "Rio Vista",
                                           Location == "SM" ~ "Montezuma",
                                           TRUE ~ Location))

cf2023 = read_excel("data/BiofoulingStudyCageRemovals.xlsx")%>%
  mutate(Condition = 100*(Weight/(ForkLength*0.1)^3)) %>%
  select(Location, Date, CageNumber, FishID, ForkLength, Weight, Condition) %>%
  mutate(Year = 2023)

  
cf2024 = read_excel("Data/SMSCGStudyCageRemovals.xlsx") %>%
  mutate(Weight = as.numeric(Weight),
Condition = 100*(Weight/(ForkLength*0.1)^3))%>%
  select(Location, Date, CageNumber, FishID, ForkLength, Weight, Condition) %>%
  mutate(Year = 2024)

allcf = bind_rows(cf2019f, cf2023, cf2024)

ggplot(allcf, aes(x = Location, y = Condition))+ geom_boxplot()+
  facet_wrap(~Year)
