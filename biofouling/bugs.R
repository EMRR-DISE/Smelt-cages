#look at thte biofoulidng data because I can't help myself

library(readxl)
library(tidyverse)
library(lubridate)

amphipods = read_excel("data/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx",
                       sheet = "Amph Data")
ggplot(amphipods, aes(x = Location, y = count, fill = Species)) + geom_col()

treatments = data.frame(Location = c("Cage 1", "Cage 2", "Cage 3", "Cage 4",
                                     "Cage 5", "Cage 6", "Cage 7", "Cage 8"),
                        Treatment = c("Flip", "Scrub", "Flip", "Scrub",
                                      "Flip", "Scrub", "Flip", "Scrub"))
amphipods = left_join(amphipods, treatments)

ggplot(amphipods, aes(x = Location, y = count, fill = Species)) + geom_col()+
  facet_wrap(Site~Treatment, scales = "free_x")


#amphipod community matrix

amphwide = pivot_wider(amphipods, id_cols = c(Location, Date, Site), names_from = Species, values_from = total_count,
                       values_fn = sum, values_fill =0)

###################################################################


zoops = read_excel("data/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx",
                       sheet = "Zoop Data") 

zoops = mutate(zoops, InOut = case_when(str_detect(Location, "Outside") ~ "Outside",
                                        str_detect(Location, "Inside") ~ "Inside"),
               CageNum = str_sub(Location, 1, 6)) %>%
  left_join(treatments, by = c("CageNum" = "Location")) %>%
  mutate(Treatment = case_when(InOut == "Outside" ~ "Outside",
                               TRUE ~ Treatment),
         Week = week(Date),
         SampleID = paste(CageNum, Date, InOut))

#this is misleading....
ggplot(zoops, aes(x = CageNum, y = Count, fill = `Species Name`)) + geom_col()+
  facet_wrap(Site~Treatment, scales = "free_x")

#calculate average CPUE of each taxa
zoopavetaxa = pivot_wider(zoops, id_cols = c(SampleID, CageNum, Treatment, Date, InOut, Site),
                          names_from = `Species Name`, values_from = Count, values_fill = 0)

zoopwzeros = pivot_longer(zoopavetaxa, cols = `Copepodid UNID nauplii`:last_col(), 
                          names_to = "Taxa", values_to = "Count")

zoopavetaxa2 = group_by(zoopwzeros, Treatment, InOut, Site, Taxa) %>%
  summarize(Count = mean(Count))
ggplot(zoopavetaxa2, aes(x = Treatment, y = Count, fill = Taxa)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")


ggplot(zoopavetaxa2, aes(x = Treatment, y = Count, fill = Taxa)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")

#rare things as 'other'

zooptax = group_by(zoopwzeros, Taxa) %>%
  summarize(totcount = sum(Count))

raretax = zooptax$Taxa[which(zooptax$totcount <40)]

zoopavetaxa2b = zoopwzeros %>%
  mutate(Tax2 = case_when(Taxa %in% raretax~ "Other",
                           TRUE ~ Taxa) ) %>%
  group_by(SampleID, Treatment, InOut, Site, Tax2) %>%
  summarize(Count = sum(Count))%>%
  group_by(Treatment, InOut, Site, Tax2) %>%
  summarize(Count = mean(Count))

library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white")

ggplot(zoopavetaxa2b, aes(x = Treatment, y = Count, fill = Tax2)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")+
  scale_fill_manual(values = mypal)


#take out rotifers and copepod nauplii

zoopavetaxa2bx = filter(zoopwzeros, !str_detect(Taxa, "nauplii"), !str_detect(Taxa, "Rotifer"),
                        !str_detect(Taxa, "Euchlanis"), !str_detect(Taxa, "Keratella"), !str_detect(Taxa, "Lecane"))



zooptaxa = group_by(zoopavetaxa2bx, Taxa) %>%
  summarize(totcount = sum(Count))

raretax = zooptaxa$Taxa[which(zooptaxa$totcount <20)]

zoopavetaxa2b = zoopavetaxa2bx %>%
  mutate(Tax2 = case_when(Taxa %in% raretax~ "Other",
                          TRUE ~ Taxa) ) %>%
  group_by(SampleID, Treatment, InOut, Site, Tax2) %>%
  summarize(Count = sum(Count))%>%
  group_by(Treatment, InOut, Site, Tax2) %>%
  summarize(Count = mean(Count))


ggplot(zoopavetaxa2b, aes(x = Treatment, y = Count, fill = Tax2)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")+
  scale_fill_manual(values = mypal)


ggplot(zoopavetaxa2, aes(x = Treatment, y = Count, fill = Taxa)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")

#just plot average total CPUE
zoopave = group_by(zoops, Treatment, InOut, Site, CageNum, Week) %>%
  summarize(CPUE = sum(Count)) %>%
  group_by(Treatment, InOut, Site, Week) %>%
  summarize(CPUE = mean(CPUE))
  

ggplot(zoopave, aes(x = Treatment, y = CPUE)) + geom_col()+
  facet_wrap(Site~Week, scales = "free_x")

zoopave2 = group_by(zoopave, Treatment, Site) %>%
  summarize(CPUEm = mean(CPUE), secpue = sd(CPUE))



ggplot(zoopave2, aes(x = Treatment, y = CPUEm)) + geom_col()+
  facet_wrap(Site~., scales = "free_x")+
  geom_errorbar(aes(ymin = CPUEm-secpue, ymax = CPUEm+secpue))

###########################################################

diets = read_excel("data/DSM Cage Diets 2019-2023.xlsx", sheet = "2023")

ggplot(diets, aes(x = Tag, y = Count, fill = `Prey Taxa`)) + geom_col()+
  facet_wrap(Treatment~Site, scales = "free_x")

ggplot(diets, aes(x = Tag, y = Count, fill = `Prey Taxa`)) + geom_col(position = "fill")+
  facet_wrap(Treatment~Site, scales = "free_x")

weights = group_by(diets, Treatment, Site, Tag) %>%
  summarize(Weight = mean(`Total Contents Weight`))


ggplot(weights, aes(x = Tag, y = Weight)) + geom_col()+
  facet_wrap(Treatment~Site, scales = "free_x")


ggplot(weights, aes(x = Site, y = Weight)) + geom_boxplot()+
  facet_wrap(~Treatment, scales = "free_x")

hist(weights$Weight)

#transform wieght and change units so it modles better

weights = mutate(weights, Weight_ug = Weight*1000, logweight = log(Weight_ug+1))

hist(weights$Weight_ug)
hist(weights$logweight)

#linear model to look for differnces by weight
dietw = lm(logweight ~ Treatment + Site, data = weights)
plot(dietw)
summary(dietw)
#no significant difference, OK.

#########################################################################
#to integrate the datasets, I first exported a csv of all the unique taxon names and built a crosswalk table
dietbugs = unique(diets$`Prey Taxa`)
write.csv(dietbugs, "data/dietbugs_2023.csv")
amphbugs = unique(amphipods$Species)
write.csv(amphbugs, "data/amphipodtaxa.csv")
zoopbugs = unique(zoops$`Species Name`)
write.csv(zoopbugs, "data/zooptaxa.csv")

crosswalk = read_csv("data/crosswalk.csv")

#now let's integrate diet and zooplankton data

#Once I built the crosswalk, I joined the common names to each dataset
diets2 = left_join(diets, select(crosswalk, Diet, Analy), by = c("Prey Taxa" = "Diet"))

diets2.1 = group_by(diets2, Analy, Site, Treatment, `Cage ID`, Tag) %>%
  summarize(Count = sum(Count, na.rm = T)) %>%
  mutate(CageNum = paste("Cage", `Cage ID`), SampleID = Tag) %>%
  mutate(Type = "Diet")

ggplot(diets2.1, aes(x = CageNum, y = Count, fill = Analy)) + geom_col(position = "fill")+
  facet_wrap(Site~Treatment, scales = "free_x")+
  scale_fill_manual(values = mypal)

  

zoops2 = left_join(zoops,select(crosswalk, Zooplankton, Analy), by = c("Species Name" = "Zooplankton") )

zoops2.1 = group_by(zoops2, Analy, Site, CageNum, Treatment, SampleID) %>%
  summarize(Count = sum(Count, na.rm =T)) %>%
  mutate(Type = "Zooplankton")

amps2 = left_join(amphipods, select(crosswalk, Amphipod, Analy), by = c("Species" = "Amphipod")) %>%
  rename(CageNum = Location) %>%
  mutate(SampleID =CageNum)

amps2.1 = group_by(amps2, Analy, Site, CageNum, Treatment, SampleID) %>%
  summarize(Count = sum(total_count, na.rm = T)) %>%
  mutate(Type = "Amphipods")

#bind all the datasets togther

Allbugs = bind_rows(amps2.1, zoops2.1, diets2.1) %>%
  select(-Tag, -`Cage ID`)

library(RColorBrewer)

#quick graph of the raw data

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "tomato", "darkgreen", "yellow")
ggplot(filter(Allbugs, Treatment != "Outside", !is.na(Analy)), aes(x = Type, y = Count, fill = Analy)) + geom_col(position = "fill") +
  facet_wrap(Site~Treatment)+
  scale_fill_manual(values = mypal)

#maybe I should standardize relative abundance by sample first

Allbugs_wide = pivot_wider(arrange(Allbugs, Analy), id_cols = c(Type, Treatment, Site, CageNum, SampleID), 
                           names_from = Analy, values_from = Count, values_fill =0) 


AllbugsRA = Allbugs_wide[,6:25]/rowSums(Allbugs_wide[,6:25]) 
AllbugsRA1 = bind_cols( Allbugs_wide[,1:5], AllbugsRA)

AllbugsRA2 = pivot_longer(AllbugsRA1, cols = c(Amphipoda:last_col()), names_to = "Species", values_to = "RelativeAbundance") %>%
  filter(Species != "NA")

ggplot(filter(AllbugsRA2, Treatment != "Outside"), aes(x = Type, y = RelativeAbundance, fill = Species)) + geom_col(position = "fill") +
  facet_wrap(Site~Treatment)+
  scale_fill_manual(values = mypal)

#not a huge difference

#take out rotifers and nauplii, since htey aren't really eaten

ggplot(filter(AllbugsRA2, Treatment != "Outside", Species != "Rotifer", Species != "Copepod nauplii"), 
       aes(x = Type, y = RelativeAbundance, fill = Species)) + geom_col(position = "fill") +
  facet_wrap(Site~Treatment)+
  scale_fill_manual(values = mypal)

ggplot(filter(AllbugsRA2, Treatment != "Outside", Species != "Rotifer", Species != "Copepod nauplii"), 
       aes(x = CageNum, y = RelativeAbundance, fill = Species)) + geom_col(position = "fill") +
  facet_wrap(Site~Type, scales = "free_x")+
  scale_fill_manual(values = mypal)
##############################################################################
library(vegan)
#permanova comparing the three data type, two  sites, and two treatments
datamat = filter(AllbugsRA2, Treatment != "Outside", Species != "Rotifer", Species != "Copepod nauplii") %>%
  pivot_wider(names_from = Species, values_from = RelativeAbundance) 

datamat = datamat[which(rowSums(datamat[,6:22]) !=0),]

foo = as.matrix(datamat[,6:22])
adonis2(foo ~ Treatment + Site + Type, data = datamat, na.rm = TRUE)




bugsMDS = metaMDS(foo)

bdata.scores <- as_tibble(scores(bugsMDS, display = "sites"))
bdata.sps <- bugsMDS$species
bdata.sps2 =  mutate(as.data.frame(bdata.sps), Species = row.names(bdata.sps))
# Combine metadata with NMDS data scores to plot in ggplot
bmeta <- cbind(datamat[,1:5], bdata.scores)
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


############################################################





#now just the diets
datamatdiet = filter(AllbugsRA2, 
                     Type == "Diet") %>%
  pivot_wider(names_from = Species, values_from = RelativeAbundance) 

datamatdiet = datamat[which(rowSums(datamatdiet[,6:22]) !=0),]
colSums(datamatdiet[,6:22])


foodiet = as.matrix(datamatdiet[,6:22])
adonis2(foodiet ~ Treatment + Site, data = datamatdiet, na.rm = TRUE)

dietMDS = metaMDS(foodiet)

data.scores <- as_tibble(scores(dietMDS, display = "sites"))
data.sps <- dietMDS$species
 data.sps2 =  mutate(as.data.frame(data.sps), Species = row.names(data.sps))
# Combine metadata with NMDS data scores to plot in ggplot
meta <- cbind(datamatdiet[,1:5], data.scores)
# Read in years as a character otherwise it shows up as a number and gets displayed as a gradient

ggplot(meta,
       aes(x = NMDS1, y = NMDS2, fill = Site)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  stat_ellipse(aes(color = Site)) +
  labs(color = "Site",
       x = NULL,
       y = NULL)

ggplot(meta,
       aes(x = NMDS1, y = NMDS2, fill = Treatment, shape = Site)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  scale_color_manual(values = c("tomato", "steelblue"))+
  scale_fill_manual(values = c("tomato", "steelblue"))+
  stat_ellipse(aes(linetype = Site, color = Treatment)) +
  geom_text(data = data.sps2, aes(x = MDS1, y = MDS2, label = Species), inherit.aes = FALSE)+
  theme_bw()+
  labs(color = "Site",
       x = NULL,
       y = NULL)
