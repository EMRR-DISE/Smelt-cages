#This analysis is to compare the zooplankton assemblage from the biofouling experiment from
#inside of the cages to outside of the cages to the FMWT catches

library(rstatix)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(vegan)
library(readxl)
library(ggpubr)
library(dplyr)

#setwd("~/Delta Smelt/biofouling coding")

# Read in Cage Data -------------------------------------------------------
zoops = read_excel("biofouling/ICF_zoops_data_cages.xlsx",
                   sheet = "Zoop Data") 

zoops$Total_Count <- (zoops$`Sample Volume (mL)`/(zoops$`Pipette Volume (mL)`*zoops$`# of Subsamples`))*zoops$Count

#Get data ready to be visualized with our naming conventions
treatments = data.frame(Location = c("Cage 1", "Cage 2", "Cage 3", "Cage 4",
                                     "Cage 5", "Cage 6", "Cage 7", "Cage 8"),
                        Treatment = c("Flip", "Scrub", "Flip", "Scrub",
                                      "Flip", "Scrub", "Flip", "Scrub"))

zoops = mutate(zoops, InOut = case_when(str_detect(Location, "Outside") ~ "Outside",
                                        str_detect(Location, "Inside") ~ "Inside"),
               CageNum = str_sub(Location, 1, 6)) %>%
  left_join(treatments, by = c("CageNum" = "Location")) %>%
  mutate(Treatment = case_when(InOut == "Outside" ~ "Outside",
                               TRUE ~ Treatment),
         Week = week(Date),
         SampleID = paste(CageNum, Date, InOut))
#Adjust for subsampling
#########Don't forget to convert to CPUE
#zoops$Total_Count <- (zoops$`Sample Volume (mL)`/(zoops$`Pipette Volume (mL)`*zoops$`# of Subsamples`))*zoops$Count

# Read in station data ----------------------------------------------------
stations <- read.csv("biofouling/SMSCG_CBNet_2018to2023CPUE_15Jul2024.csv")
#pull station numbers of interest (get comprehensive list from christina)
interest <- c("606","609","MONT","706","707","708","709","710")
stn <- data.frame()
for (i in 1:length(interest)) {
  a <- interest[i]  
  b <- stations[which(stations$Station == a),]
  stn <- rbind(stn, b)
}
stn2023 <- stn[which(stn$Year == 2023),]
#Stations 708 or 709 do not have any data

stn_long <- gather(stn2023, key = 'Prey', value = 'CPUE', ACARTELA:CUMAC)
#exclude all of the preys that start with "ALL..." because they are the sum of other columns i.e. double counting
#does that same thing stand for "OTH..." category?

#Probably need to filter down dates because it includes the full year

# Crosswalk zoop data --------------------------------------------

#Use crosswalk list Rosie made
crosswalk = read_csv("biofouling/crosswalk_wstns.csv")
biomass <- read.csv("biofouling/Cage Biomass crosswalk1.csv")
masscrosswalk_zoops <- inner_join(crosswalk, biomass, by = c(Zooplankton = "CageCode"))
masscrosswalk_stations <- inner_join(crosswalk, biomass, by = c(EMP_Code = "EMPCode"))

#Once I built the crosswalk, I joined the common names to each dataset
zoops2 = left_join(zoops, select(masscrosswalk_zoops, Zooplankton, Analy, Carbon_weight_ug), by = c("Species Name" = "Zooplankton") )

zoops2.1 = group_by(zoops2, Location, Analy, Site, Date, CageNum, Treatment, SampleID) %>%
  summarize(Count = sum(Total_Count, na.rm =T), 
            Mass = sum((Total_Count/0.0378541)*Carbon_weight_ug, na.rm =T),
            CPUE = sum(Total_Count/0.0378541)) %>%
  mutate(Type = "Zooplankton") #CPUE*Biomass catch/cubic meter

#add this bit for graphing only
#zoops2.1[nrow(zoops2.1)+1,] <- NA
#a <- nrow(zoops2.1)
#zoops2.1[a,1] <- "Cumacean"
#zoops2.1[a,4] <- "Scrub"
#zoops2.1[a,8] <- "Zooplankton"
#zoops2.1[a,6] <- 0
#zoops2.1[a,2] <- "Rio Vista"

stn2 = left_join(stn_long, select(masscrosswalk_stations, EMP_Code, Analy, Carbon_weight_ug), 
                 by = c("Prey" = "EMP_Code"))

stn2.1 = stn2 %>%
  mutate(SampleID = paste(Station, Date)) %>%
  #group_by(Year, Station, Date, Analy, CPUE) %>% Scott's origional code
  group_by(Year, Station, Date, Analy, SampleID) %>% #Rosie's edit
  summarize(CPUE = sum(CPUE, na.rm = T), Mass = sum(CPUE*Carbon_weight_ug, na.rm =T)) %>%
  mutate(Type = "Station")

stn2.1 <- stn2.1[which(stn2.1$Analy != "Ignore"),]

#add this bit for graphing only
#miss <- c("Annelida", "Corophiidae", "Gammaridea", "Gastropod", "Hyallela", "Insect")

#for (i in 1:length(miss)) {
#  stn2.1[nrow(stn2.1)+1,] <- NA
#  a <- nrow(stn2.1)
#  stn2.1[a,4] <- miss[i]
#  stn2.1[a,6] <- "Station"
#  stn2.1[a,2] <- "MONT"
#  stn2.1[a,5] <- 0
#  stn2.1[a,3] <- "7/11/2023"
#  stn2.1[a,1] <- 2023
#}
#tail(stn2.1)

#quick graph of the raw data
#collected data graph
ggplot(zoops2.1, aes(x = Treatment, y = Count, fill = Analy)) + 
  geom_col(position = "fill") +
  facet_wrap(~Site)

ggplot(zoops2.1, aes(x = SampleID, y = Count, fill = Analy)) + 
  geom_col() +
  facet_wrap(~Site)

ggplot(zoops2.1, aes(x = SampleID, y = Mass, fill = Analy)) + 
  geom_col() +
  facet_wrap(~Site)



ggplot(zoops2.1, aes(x = Treatment, y = Mass, fill = Analy)) + 
  geom_col(position = "fill") +
  facet_wrap(~Site)

#Station data graph
ggplot(stn2.1, aes(x = Station, y = CPUE, fill = Analy)) + 
  geom_col(position = "fill") +
  facet_wrap(~Date) 

ggplot(stn2.1, aes(x = Station, y = Mass, fill = Analy)) + 
  geom_col(position = "fill") +
  facet_wrap(~Date) 

#Combine data to get at comparisons
head(zoops2.1)
head(stn2.1)

#Rosie's version
zoops2.2 = zoops2.1 %>%
  ungroup() %>%
  select(SampleID, Site, Treatment, CPUE, Mass, 
         Type, CageNum, Analy, Date)

#zoops2.2 <- zoops2.1$Date
#zoops2.2 <- as_tibble(zoops2.2)
#zoops2.2[,2] <- zoops2.1$Site
#zoops2.2[,3] <- zoops2.1$Treatment
#zoops2.2[,4] <- zoops2.1$Analy
#zoops2.2[,5] <- zoops2.1$Count #should this have been CPUE?
#zoops2.2[,6] <- zoops2.1$Mass

#Rosie's edit
stn2.2 = stn2.1 %>%
  ungroup() %>%
  mutate(Date = mdy(Date)) %>%
  select(SampleID, Date, Type, Analy, CPUE, Mass, Station) %>%
  mutate(Treatment = "FMWT", Site = case_when(Station %in% c("606", "609", "MONT") ~ "Montezuma",
                                              Station %in% c("706", "707") ~ "Rio Vista"))

#stn2.2 <- stn2.1$Date 
#stn2.2 <- as_tibble(stn2.2)
#stn2.2[,2] <- stn2.1$Station
#stn2.2[,3] <- stn2.1$Type
#stn2.2[,4] <- stn2.1$Analy
#stn2.2[,5] <- stn2.1$CPUE
#stn2.2[,6] <- stn2.1$Mass

#cnames <- c("Date", "Site", "Type", "Species", "CPUE", "Mass")

#colnames(zoops2.2) <- cnames
#colnames(stn2.2) <- cnames

#alloutbugs <- rbind(stn2.2, zoops2.2)

alloutbugs <- bind_rows(stn2.2, zoops2.2) %>%
  rename(Species = Analy)
alloutbugs <- filter(alloutbugs, Date > ymd("2023-08-20"))

ggplot(filter(alloutbugs, Treatment != "Scrub" & Treatment != "Flip"), aes(x = Treatment, y = CPUE, fill = Species)) + 
  geom_col(position = "fill") +
  facet_wrap(~Site)


ggplot(alloutbugs, aes(x = SampleID, y = CPUE, fill = Species)) + 
  geom_col() +
  facet_wrap(~Site, scales = "free_x")


ggplot(alloutbugs, aes(x = SampleID, y = Mass, fill = Species)) + 
  geom_col() +
  facet_wrap(~Site, scales = "free_x")

ggplot(filter(alloutbugs, Type != "Scrub" & Type != "Flip"), aes(x = Site, y = Mass, fill = Species)) + 
  geom_col(position = "fill") 

allinterestingbugs <- filter(alloutbugs, Species != "Hyalella", Species != "Gammaridea", Species != "Corophiidae",
                             Species != "Insect", Species != "Annelida", Species != "other")
save(allinterestingbugs, file = "data/allinterestingbugs.RData")
#We probably want to plot average CPUE rather than total
InterestingAve = allinterestingbugs %>%
  group_by(Treatment, Site, Species) %>%
  summarize(CPUE = mean(CPUE), Mass = mean(Mass))


ggplot(filter(allinterestingbugs, Type != "Scrub" & Type != "Flip"), 
       aes(x = Treatment, y = CPUE, fill = Species)) + 
  geom_col(position = "fill") +
  facet_wrap(Site~Date) 

ggplot(filter(InterestingAve), 
       aes(x = Treatment, y = Mass, fill = Species)) + 
  geom_col(position = "fill") +
  facet_wrap(~Site)
  

#Make new groupings to compare 
# allinterestingbugs$Group <- "Place"
# allinterestingbugs[which(allinterestingbugs$Site == "606"),7] <- "Montezuma"
# allinterestingbugs[which(allinterestingbugs$Site == "609"),7] <- "Montezuma"
# allinterestingbugs[which(allinterestingbugs$Site == "MONT"),7] <- "Montezuma"
# allinterestingbugs[which(allinterestingbugs$Site == "Montezuma"),7] <- "Montezuma"
# allinterestingbugs[which(allinterestingbugs$Site == "706"),7] <- "Rio Vista"
# allinterestingbugs[which(allinterestingbugs$Site == "707"),7] <- "Rio Vista"
# allinterestingbugs[which(allinterestingbugs$Site == "Rio Vista"),7] <- "Rio Vista"



ggplot(filter(InterestingAve,  Species == "Pseudodiaptomus forbesi"), 
       aes(x = Site, y = log(Mass), fill = Treatment)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Pseudodiaptomus forbesi")

ggplot(filter(allinterestingbugs, Species == "Pseudodiaptomus forbesi"), 
       aes(x = Site, y = CPUE, fill = Treatment)) + 
  geom_boxplot(position = "dodge") +
  labs(title = "Pseudodiaptomus forbesi")

ggplot(filter(InterestingAve, Treatment != "Scrub" &  Treatment  != "Flip", Species == "Pseudodiaptomus forbesi"), 
       aes(x = Site, y = CPUE, fill = Treatment)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Pseudodiaptomus forbesi")

ggplot(filter(InterestingAve,  Treatment != "Scrub" &  Treatment  != "Flip",Species == "Pseudodiaptomus nauplii"), 
       aes(x = Treatment, y = Mass, fill =Site)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Pseudodiaptomus nauplii")

ggplot(filter(InterestingAve,  Treatment != "Scrub" &  Treatment  != "Flip", Species == "Pseudodiaptomus nauplii"), 
       aes(x = Treatment, y = CPUE, fill = Site)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Pseudodiaptomus nauplii")

ggplot(filter(InterestingAve, Treatment != "Scrub" & Treatment != "Flip", Species == "Limnoithona"), 
       aes(x = Site, y = CPUE, fill = Treatment)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Limnoithona")

ggplot(filter(InterestingAve,  Species == "Limnoithona"), 
       aes(x = Treatment, y = Mass, fill = Site)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Limnoithona")

#This is the NMDS, I dont see the permanova
vegbugs <- dist(alloutbugs$CPUE)
zoop_dist <- vegdist(vegbugs, method = "bray")
nmds <- metaMDS(zoop_dist)

stressplot(nmds)

scores(nmds)%>%
  as_tibble(rownames == "Group") %>%
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point()

#########################################################
#ANOVAs of Limnoithona abundance, P forbesi abundance, and p nauplii abundance
#But I have to add the zeros in first!!

allwideCPUE = pivot_wider(allinterestingbugs, id_cols = c(SampleID, Date, Type, Treatment, Site),
                      names_from = Species, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = c(Calanoid_other:last_col()), names_to = "Species", values_to = "CPUE")

limno = filter(allwideCPUE, Species == "Limnoithona")
limlm = lm(log(CPUE+1) ~ Treatment + Site, data = limno)
summary(limlm)
anova(limlm)
emmeans(limlm, pairwise ~ Treatment)

pfor = filter(allwideCPUE,
              Species == "Pseudodiaptomus forbesi")
pforlm = lm(log(CPUE+1) ~ Treatment + Site, data = pfor)
summary(pforlm)
Anova(pforlm)
emmeans(pforlm, pairwise ~ Treatment)
emmeans(pforlm, pairwise ~ Site)
plot(allEffects(pforlm))
