#permanova comparing diet bug biomss, amphipod biomass, and zoop biomass

library(vegan)
library(tidyverse)
library(readxl)


library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white", "orangered", "black")
#Diet bioamss data
Dietbm = read_csv("data/cagedietbiomass.csv")%>%
  bind_rows(empties)
Crosswalk = read_csv("data/crosswalk 1.csv")

Dietbmx = mutate(Dietbm, `Taxa Group` = case_when(CageCode == "Chironomidae larva"  ~ "Chironomid",
                                  CageCode == "Copepod nauplius" ~ "Copepod nauplii",
                                  CageCode == "Tanaid" ~ "Tanaid",
                                  TRUE ~ `Taxa Group`)) %>%
  rename(Site = Location, CageID = `Cage ID`) %>%
  group_by(Site, Treatment, FishID, `Taxa Group`) %>%
  summarize(Biomass = sum(Biomass), Count = sum(Count)) %>%
  mutate(Type = "Diet")

#amphipod data
load("data/amph_sum_counts.RData")

amph_sum = rename(amph_sum_counts, CageID = Location, Biomass = Biomass_median) %>%
  mutate(`Taxa Group` = case_when(NewName == "Chironimidae" ~ "Chironomid",
                                  NewName == "Tanaidacea" ~ "Tanaid",
                                  TRUE ~ NewName),
         Type = "Biofouling")
#now the zooplankton


zoops = read_excel("data/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx",
                   sheet = "Zoop Data") 

zoops = mutate(zoops, uniqueID = paste(Date, Location))

zoops = mutate(zoops, InOut = case_when(str_detect(Location, "Outside") ~ "Outside",
                                        str_detect(Location, "Inside") ~ "Inside"),
               CageNum = str_sub(Location, 1, 6)) %>%
  left_join(treatments, by = c("CageNum" = "Location")) %>%
  mutate(Treatment = case_when(InOut == "Outside" ~ "Outside",
                               TRUE ~ Treatment),
         Week = week(Date),
         SampleID = paste(CageNum, Date, InOut),
         totalCount = Count*`Sample Volume (mL)`/`# of Subsamples`) 

crossbm = read_csv("data/Cage Biomass crosswalk1.csv")

zoopsx = left_join(zoops, crossbm, by = c("Species Name" = "CageCode")) %>%
  mutate(Biomass = totalCount* Carbon_weight_ug) 

zoops2.1bm = group_by(zoopsx, `Taxa Group`, Site, CageNum, Treatment, SampleID) %>%
  summarize(Biomass = sum(Biomass, na.rm =T), Count = sum(totalCount)) %>%
  mutate(Type = "Zooplankton") %>%
  rename(CageID = CageNum)

#all bugs by biomass

Allbugsbm = bind_rows(amph_sum, zoops2.1bm, Dietbmx) %>%
  mutate(Treatment = case_when(Treatment == "Flip" ~ "Exchanged",
                               Treatment == "Scrub" ~ "Scrubbed",
                               TRUE ~ Treatment)) %>%
  mutate(Site = case_when(Site == "Belden's Landing" ~ "Montezuma",
                          TRUE ~ Site)) %>%
  filter(Treatment != "Outside", `Taxa Group` != "Ignore")

ggplot(Allbugsbm , aes(x = Type, y = Biomass, fill = `Taxa Group`)) + geom_col(position = "fill")+
  facet_grid(Site~Treatment)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Biomass")+
  xlab(NULL)

ggplot(Allbugsbm , aes(x = Type, y = Count, fill = `Taxa Group`)) + geom_col(position = "fill")+
  facet_grid(Site~Treatment)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  xlab(NULL)

#Now the statistics

#permanova comparing the three data type, two  sites, and two treatments
datamatbm = filter(Allbugsbm, Treatment != "Outside", `Taxa Group` != "Empty") %>%
  pivot_wider(names_from = `Taxa Group`, values_from = Biomass, values_fill = 0) 

datamatbm = datamatbm[which(rowSums(datamatbm[,11:26]) !=0),]

matbm= as.matrix(datamatbm[,11:26])
matbmRA = matbm/rowSums(matbm) 

 
a2 = adonis2(matbm ~ Treatment + Site*Type, data = datamatbm, na.rm = TRUE)
a2
write.csv(a2, "biofouling/dietzoop_permanovaBM.csv")


#Count version
datamatc = filter(Allbugsbm, Treatment != "Outside", `Taxa Group` != "Empty") %>%
  pivot_wider(names_from = `Taxa Group`, values_from = Count, values_fill = 0) 

datamatc = datamatc[which(rowSums(datamatc[,11:26]) !=0),]

matc= as.matrix(datamatc[,11:26])
matcRA = matc/rowSums(matc) 


a3 = adonis2(matc ~ Treatment + Site*Type, data = datamatc, na.rm = TRUE)
a3
write.csv(a3, "biofouling/dietzoop_permanova_count.csv")


#This MDS is having issues, probably because there are groups that don't appear in all types
bugsMDS = metaMDS(matbmRA, try = 100, trymax = 100)
#stress is nearly zero, you may have insufficient data

bdata.scores <- as_tibble(scores(bugsMDS, display = "sites"))
bdata.sps <- bugsMDS$species
bdata.sps2 =  mutate(as.data.frame(bdata.sps), Species = row.names(bdata.sps))
# Combine metadata with NMDS data scores to plot in ggplot
bmeta <- cbind(datamatbm[,1:10], bdata.scores)
# Read in years as a character otherwise it shows up as a number and gets displayed as a gradient

ggplot(bmeta,
       aes(x = NMDS1, y = NMDS2, fill = Type)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  stat_ellipse(aes(color = Site)) +
  labs(color = "Site",
       x = NULL,
       y = NULL)

ggplot(bmeta,
       aes(x = NMDS1, y = NMDS2, fill = Type, shape = Site)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  
  stat_ellipse(aes(linetype = Site, color = Type)) +
  geom_text(data = bdata.sps2, aes(x = MDS1, y = MDS2, label = Species), inherit.aes = FALSE)+
  scale_color_manual(values = c("tomato", "steelblue", "yellow3"))+
  scale_fill_manual(values = c("tomato", "steelblue", "yellow3"))+ 
  theme_bw()+
  labs(color = "Site",
       x = NULL,
       y = NULL)
