#compare all three years

library(tidyverse)
library(readxl)
library(readxl)
library(emmeans)

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

cfall = read_excel("data/19-24 Cage Growth_allfish.xlsx") %>%
  filter(!is.na(Year)) 

cfall = mutate(cfall, ID = str_remove(OrigionalID, "2023")) %>%
  mutate(ID = str_remove(ID, "2024")) %>%
  rename(FishID = ID, NewID = OrigionalID)

write.csv(cfall, "cfall.csv", row.names = F)


########survival#####################

allsurv = read_excel("data/19-24 Cage Survival.xlsx")
surv2024 = filter(allsurv, Year == 2024, Site != "FCCL")

survlm = lm(PercSurv ~ Site, data = surv2024)
summary(survlm)
emmeans(survlm, pairwise ~ Site)


#####################################################

#sort out these fish ID's.

diet2019 = filter(diet, Year ==2019) %>%
  select(Year, Date, Tag) %>%
  distinct()
both = cf2019[which(cf2019$ID %in% diet2019$Tag),]
notboth = diet2019[which(!diet2019$Tag %in% cf2019$ID),]
bothdiet = diet2019[which(diet2019$Tag %in% cf2019$ID),]


library(pwr)

########reall all the data #####################

allfish = read_excel("data/19-24 Cage Growth_allfish.xlsx")

allfishpre1a = filter(allfish, Site ==  "Pre-deployed", Season %in% c("Summer", "Fall")) %>%
  mutate(Site ="RV")
allfishpre2 = filter(allfish, Site ==  "Pre-deployed", Season %in% c("Summer", "Fall")) %>%
  mutate(Site ="BDL")
allfishpre3 = filter(allfish, Site ==  "Pre-deployed", Season %in% c("Summer", "Fall")) %>%
  mutate(Site ="FCCL")


allfishnew = filter(allfish, Site != "Pre-deployed") %>%
  bind_rows(allfishpre1a, allfishpre3, allfishpre2) %>%
  mutate(group = paste(Year, Period, Site), Weight = as.numeric(Weight_g),
         date = case_when(Date == ymd("2019-06-19") ~ ymd("2019-11-06"),
                          TRUE ~ Date)) %>%
  filter(Season %in% c("Summer", "Fall"), Site %in% c("RV", "BDL", "FCCL", "Pre-deployed"),
         Date > ymd("2019-10-01")) 

ggplot(allfishnew,  aes(x = Weight, fill = Period))+ geom_density(alpha = 0.5)+
  facet_grid(Site~Year)
