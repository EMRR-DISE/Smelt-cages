#permanova comparing diet bug biomss, amphipod biomass, and zoop biomass

library(vegan)
library(tidyverse)
library(readxl)


library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white", "orangered", "black")
#Diet bioamss data

diets = read_excel("data/DSM Cage Diets 2019-2023.xlsx", sheet = "2023")

empties = filter(diets, `Prey Taxa`=="EMPTY") %>%
  rename(Location = Site, FishID = Tag, CageCode = `Prey Taxa`) %>%
  select(Location, Treatment, `Cage ID`, FishID, Fullness, Digestion, `Total Contents Weight`,
         CageCode) %>%
  mutate(Biomass =0, Count =0, Empty = "Y", Treatment = "Exchanged", Location = "Belden's Landing",
         LabWeight_g = 0.684, `Taxa Group` = "Empty")



Dietbm = read_csv("data/cagedietbiomass.csv")%>%
  bind_rows(empties) %>%
  rename(Site = Location, CageID = `Cage ID`) %>%
  group_by(Site, CageID, Treatment, FishID, `TaxaGroup`) %>%
  summarize(Biomass = sum(Biomass), Count = sum(Count)) %>%
  mutate(Type = "Diet", CageID = as.character(CageID))

#amphipod data
load("data/amph_sum_counts.RData")

amph_sum = rename(amph_sum_counts, CageID = Location, Biomass = Biomass_median) %>%
  mutate(`TaxaGroup` = case_when(NewName == "Chironimidae" ~ "Chironomid",
                                  NewName == "Tanaidacea" ~ "Tanaid",
                                  NewName == "Ignore" ~ "Other",
                                  TRUE ~ NewName),
         Type = "Biofouling")

#now the zooplankton
#from Scott's code
load("data/allinterestingbugs.RData")

#all bugs by biomass
allwzeros = rename(allwzeros, Biomass = Mass, TaxaGroup = Species, Count = CPUE)
Allbugsbm = bind_rows(amph_sum, allwzeros, Dietbm) %>%
  mutate(Treatment = case_when(Treatment == "Flip" ~ "Exchanged",
                               Treatment == "Scrub" ~ "Scrubbed",
                               TRUE ~ Treatment)) %>%
  mutate(Site = case_when(Site == "Belden's Landing" ~ "Montezuma",
                          TRUE ~ Site)) 

save(Allbugsbm, file = c("biofouling/Allbugsbm.RData"))

#Plots
#######################################################
lab = data.frame(Type = "Biofouling", Biomass = 0.9, Site = "Montezuma", Treatment = "Exchanged", label = "A")
lab2 = data.frame(Type = "Biofouling", Biomass = 0.9, Site = "Montezuma", Treatment = "Exchanged", label = "B")

RBM = ggplot(filter(Allbugsbm, !Treatment %in% c("FMWT", "Outside")),
             aes(x = Type, y = Biomass, fill = `TaxaGroup`)) + geom_col(position = "fill")+
  facet_grid(Site~Treatment)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Biomass")+
  xlab(NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_text(data = lab, aes(x = Type, y = Biomass, label = label),
            size =8, inherit.aes = FALSE) 
RBM

RA = ggplot(filter(Allbugsbm, !Treatment %in% c("FMWT", "Outside")), 
            aes(x = Type, y = Count, fill = `TaxaGroup`)) + geom_col(position = "fill")+
  facet_grid(Site~Treatment)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  theme_bw()+
  geom_text(data = lab2, aes(x = Type, y = Biomass, label = label), 
            size =8, inherit.aes = FALSE) +
  xlab(NULL)

RBM+RA

ggsave("plots/dietzoopamp_relativeabundance.tiff", width =10, height =6, device = "tiff")

ggplot(Allbugsbm , aes(x = Type, y = Count, fill = `TaxaGroup`)) + geom_col()+
  facet_grid(Site~Treatment)+
  scale_fill_manual(values = mypal)+
  ylab("Abundance")+
  xlab(NULL)



################################################
#Now the statistics

#permanova comparing the three data type, two  sites, and two treatments
datamatbm = filter(Allbugsbm, Treatment != "Outside", `TaxaGroup` != "Empty") %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `TaxaGroup`, Biomass) %>%
  pivot_wider(names_from = `TaxaGroup`, values_from = Biomass, values_fill = 0,
              values_fn = sum) 

datamatbm = datamatbm[which(rowSums(datamatbm[,7:22]) !=0),]

#matrix of biomass
matbm= as.matrix(datamatbm[,7:22])

#matrix of relative biomass
matbmRA = matbm/rowSums(matbm) 

 
a2 = adonis2(matbmRA ~ Treatment + Site+Type, data = datamatbm, na.rm = TRUE)
a2
write.csv(a2, "biofouling/dietzoop_permanovaBM.csv")


#Count version
datamatc = filter(Allbugsbm, Treatment != "Outside", `Taxa Group` != "Empty") %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `Taxa Group`, Count) %>%
  pivot_wider(names_from = `Taxa Group`, values_from = Count, values_fill = 0) 

datamatc = datamatc[which(rowSums(datamatc[,7:22]) !=0),]

matc= as.matrix(datamatc[,7:22])
matcRA = matc/rowSums(matc) 


a3 = adonis2(matcRA ~ Treatment + Site+Type, data = datamatc, na.rm = TRUE)
a3
write.csv(a3, "biofouling/dietzoop_permanova_count.csv")

#you can also try a non-metric multidimentional scaling analysis
bugsMDS = metaMDS(matbmRA, try = 100, trymax = 100)

#extract the scores for plotting
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
