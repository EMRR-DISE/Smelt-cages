#recapture data

library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(spacetools)
library(deltamapr)

# install.packages("devtools")
#devtools::install_github("sbashevkin/spacetools")

recaps = read_excel("Recaptured Experimental Release_2022-2023_working draft (2).xlsx", 
                    sheet = "Full DSM Data") %>%
  rename(Site = `Release Site`, Type = `Release Type`, Latitude= `Start Latitude`, Longitude = `Start Longitude`)
recaps    =     mutate(recaps, fishID = paste("smelt", 1:nrow(recaps)),
                Type = case_when(Type == "header" ~ "Soft",
                                 is.na(Type) ~ "Wild",
                                 TRUE ~ Type),
                Site = case_when(is.na(Site) ~ "Wild",
                                 TRUE ~ Site))
str(recaps)

recapsf = st_as_sf(filter(recaps, !is.na(`Longitude`)), coords = c("Longitude", "Latitude"), crs = 4326)

release = data.frame(Site = c("SDWSC", "SDWSC", "Rio Vista", "Rio Vista"), Type = c("Soft", "Hard", "Hard", "Hard (Trailer)"),
                     Latitude = c(38.237842, 38.275563, 38.124736, 38.139752), 
                     Longitude = c(-121.673802, -121.661374, -121.699521, -121.694706)) 
releasesf = st_as_sf(release,  coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = recapsf, aes(color = Type))+
  geom_sf(data = releasesf, aes(color = Type), size = 5)+
  
  coord_sf(xlim = c(-121.5, -122.2), ylim = c(37.8, 38.5))
  
#merge recap points with release points

recaps2 = left_join(recaps, release)

#Calculate distance with GGdist
#one release at a time
Dwschard = filter(recaps2, Type == "Hard", Site == "SDWSC")
dwsch = filter(release, Site == "SDWSC", Type == "Hard") %>%
  select(Latitude, Longitude)
tesst = GGdist(Water_map = spacetools::Delta, Points = Dwschard, Latitude_column = `Latitude`,
               Longitude_column = `Longitude`, PointID_column = fishID, EndPoint = dwsch)

#now let's do DWSC soft
Dwscsoft = filter(recaps2, Type == "Soft", Site == "SDWSC")
dwscs = filter(release, Site == "SDWSC", Type == "Soft") %>%
  select(Latitude, Longitude)
tesst2 = GGdist(Water_map = spacetools::Delta, Points = Dwscsoft, Latitude_column = `Latitude`,
               Longitude_column = `Longitude`, PointID_column = fishID, EndPoint = dwscs)

#now let's do Rio Vista hard
RVhard = filter(recaps2, Type == "Hard", Site == "Rio Vista", !is.na(Latitude))
rvh = filter(release, Site == "Rio Vista", Type == "Hard") %>%
  select(Latitude, Longitude)
tesst3 = GGdist(Water_map = spacetools::Delta, Points = RVhard, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = fishID, EndPoint = rvh)

#now the trailer
RVhardt = filter(recaps2, Type == "Hard (Trailer)", Site == "Rio Vista")
rvht = filter(release, Site == "Rio Vista", Type == "Hard (Trailer)") %>%
  select(Latitude, Longitude)
tesst4 = GGdist(Water_map = spacetools::Delta, Points = RVhardt, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = fishID, EndPoint = rvht)

distances = bind_rows(tesst, tesst2, tesst3, tesst4)

recaps3 = left_join(recaps2, distances) %>%
  mutate(SiteType = paste(Type, Site))

ggplot(recaps3, aes(x = SiteType, y = Distance))+ geom_boxplot(aes(fill = Type))+
  ylab("Distance from Release Site (m)")+
  theme_bw()

write.csv(recaps3, "Recaptures.csv")
recapsf = st_as_sf(filter(recaps3, !is.na(`Longitude`)), coords = c("Longitude", "Latitude"), crs = 4326)


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = recapsf, aes(color = Distance), size =3)+
  geom_sf(data = releasesf, aes(shape = Type), size = 5)+
  scale_color_viridis_c(option = "B")+
  coord_sf(xlim = c(-121.5, -122.2), ylim = c(37.8, 38.5))

recapsum = group_by(recaps3, Site, Type, SiteType) %>%
  summarize(Nsmelt = n(), MeanDist = mean(Distance, na.rm = T), sdDist = sd(Distance, na.rm =T))


###########################################################################3
#Brian also wants distances bewtween recaptures

RVhdist = Waterdist(Water_map = spacetools::Delta, Points = RVhardt, Latitude_column = `Latitude`,
                    Longitude_column = `Longitude`, PointID_column = fishID)

RVhdistx = Waterdist(Water_map = spacetools::Delta, Points = RVhard, Latitude_column = `Latitude`,
                    Longitude_column = `Longitude`, PointID_column = fishID)

SCsoft  = Waterdist(Water_map = spacetools::Delta, Points = Dwscsoft, Latitude_column = `Latitude`,
                       Longitude_column = `Longitude`, PointID_column = fishID)

SChard = Waterdist(Water_map = spacetools::Delta, Points = Dwschard, Latitude_column = `Latitude`,
                   Longitude_column = `Longitude`, PointID_column = fishID)

#I'm not really sure the best way to summarize this.

Cluster = data.frame(Release = c("RV Hard", "RV Hard Trailer", "DWSC soft", "DWSC hard"),
                     meadist = c(mean(RVhdistx[lower.tri(RVhdistx)]), 
                                 mean(RVhdist[lower.tri(RVhdist)]), 
                                 mean(SCsoft[lower.tri(SCsoft)]), 
                                 mean(SChard[lower.tri(SChard)])),
                     SD = c(sd(RVhdistx), sd(RVhdist), sd(SCsoft), sd(SChard)))
Cluster = mutate(Cluster, CV = SD/meadist)
write.csv(Cluster, "Cluster.csv")                     

save(distances, recaps, recaps2, recaps2x, recapsf, recaps3, release, Cluster, RVhdist, RVhdistx, SCsoft, SChard, file = "Recaps.RData")

mm1 = lm(Distance ~ Type, data = recaps3)
summary(mm1)
plot(mm1)
