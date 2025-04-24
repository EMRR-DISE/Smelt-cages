#clean copy of all the plots going in teh biofouling manuscript

library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(emmeans)
library(car)
library(vegan)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white", "orangered", "black")

load("biofouling/Allbugs.Rdata")
load("biofouling/Allbugsbm.Rdata")



#Plots
#######################################################

#This is the figure for the paper with relative abundance and relative biomass
RBM = ggplot(filter(Allbugsbm, !Treatment %in% c("Outside", "FMWT")),
                    aes(x = Treatment, y = Biomass, fill = `TaxaGroup`)) + 
  geom_col(position = "fill")+
  facet_grid(Type~Site)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Biomass")+
  xlab(NULL)+
  theme_bw()+
  theme(legend.position = "none")#+
 # geom_text(data = lab, aes(x = Type, y = Biomass, label = label),
 #          size =8, inherit.aes = FALSE) 
RBM

RA = ggplot(filter(Allbugsbm, !Treatment %in% c("Outside", "FMWT")), 
            aes(x = Treatment, y = Count, fill = `TaxaGroup`)) + 
  geom_col(position = "fill")+
  facet_grid(Type~Site)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  theme_bw()

RBM+RA

ggsave("plots/dietzoopamp_relativeabundance.tiff", width =10, height =6, device = "tiff")


##########################################################
#now the absolute abundance

AllbugsSum = group_by(Allbugsbm, `TaxaGroup`, Site, Treatment, Type) %>%
  summarize(Count = mean(Count), Biomass = mean(Biomass))

TAB = ggplot(filter(AllbugsSum, `TaxaGroup` != "Empty"), aes(x = Treatment, y = Count, fill = `TaxaGroup`)) +
  facet_grid(Type ~ Site, scale = "free_y")+
  geom_col()+
  scale_fill_manual(values = mypal)+
  ylab("Mean Number per Sample")+
  theme_bw()
  
  
TBM =   ggplot(filter(AllbugsSum, `TaxaGroup` != "Empty"), 
               aes(x = Treatment, y = Biomass, fill = `TaxaGroup`)) +
    facet_grid(Type ~ Site, scale = "free_y")+
    geom_col()+
    scale_fill_manual(values = mypal)+
    ylab("Mean Biomass (ug) per Sample")+
  theme_bw()+
  theme(legend.position = "none")

TBM+TAB


ggsave("plots/dietzoopamp_absoluteeabundance.tiff", width =10, height =6, device = "tiff")



#######################
#version with outside and FMWT samples added
allinbug = Allbugsbm %>%
  mutate(Type = case_when(Treatment %in% c("FMWT", "Outside") ~"Exterior\nZooplankton",
                          TRUE ~ Type),
         SampleID = paste(CageID, Treatment, Date, Site, FishID, Type))

Allbugs2 = allinbug %>%
  filter(`TaxaGroup` != "Empty") %>%
  mutate(`TaxaGroup` = case_when(`TaxaGroup` == "Chironomidae" ~ "Chironomid",
                                 `TaxaGroup` == "Harpaticoid" ~ "Harpacticoid",
                                 TaxaGroup %in% c("Amphipod", "Corophiidae", "Gammaridea", "Other Amphipod") ~ "Amphipod",
                                 TRUE ~ TaxaGroup)) %>%
   #                                `Taxa Group` == "Cyclopoid_other" ~ "Cyclopoid Copepod",
  #                                 `Taxa Group` %in% c("Pseudodiaptomus", "Pseudodiaptomus forbesi", 
  #                                 "Pseudodiaptomus Juvenile") ~ "Pseudodiaptomus",
  #                                 `Taxa Group` == "Pseudodiaptomus nauplii" ~ "Copepod nauplii",
  #                                 `Taxa Group` == "Harpaticoida" ~ "Harpacticoid",
  #                                 ,
  #                                 `Taxa Group` == "Cladocera" ~ "Cladoceran",
  #                                 TRUE ~ `Taxa Group`)) %>%
  mutate(Type = factor(Type, levels = c("Diet", "Biofouling", "Zooplankton", "Exterior\nZooplankton"),
                       labels =  c("Smelt Diet", "Biofouling\nMacroinvertebrates", "Interior\nZooplankton", 
                                   "Exterior\nZooplankton"))) %>%
  mutate(Taxa = factor(`TaxaGroup`, levels = c( "Eurytemora","Pseudodiaptomus",
                                               "Pseudodiaptomus nauplii","Other Calanoid",
                                                "Limnoithona","Other Cyclopoid",
                                               "Copepod nauplii", "Harpacticoid",
                                                "Cladoceran","Rotifer", "Amphipod",
                                               "Isopod", "Cumacean" ,
                                                "Insect", "Chironomid", "Gastropod",
                                                "Ostracod", "Tanaid", "Other")),
         Location = factor(Site, levels = c("Montezuma", "Rio Vista"), labels = c("Belden's Landing", "Rio Vista")))

RBM2 = ggplot(Allbugs2 , aes(x = Treatment, y = Biomass, fill = Taxa)) + 
  geom_col(position = "fill")+
  facet_grid(Location ~ Type, scales = "free_x")+
  scale_fill_manual(values = mypal)+
  ylab("Relative Biomass")+
  xlab(NULL)+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())#+
# geom_text(data = lab, aes(x = Type, y = Biomass, label = label),
#          size =8, inherit.aes = FALSE) 
RBM2

RA2 = ggplot(Allbugs2 , aes(x = Treatment, y = Count, fill = Taxa)) + 
  geom_col(position = "fill")+
  facet_grid(Location ~ Type, scales = "free_x")+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  xlab(NULL)+
  theme_bw()+
  theme(legend.position = "bottom")

RA2

RBM2/RA2



ggsave("plots/dietzoopamp_barchart_wfmwt.tiff", width =7, height =7, device = "tiff")


################################################
#Now the statistics

#permanova comparing the three data type, two  sites, and two treatments
datamatbm = filter(Allbugsbm, Treatment != "Outside", Treatment != "FMWT", `TaxaGroup` != "Empty") %>%
  mutate(SampleID = paste(CageID, Treatment, Date, Site, FishID)) %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `TaxaGroup`, Biomass, SampleID) %>%
  pivot_wider(names_from = `TaxaGroup`, values_from = Biomass, values_fill = 0,
              values_fn = sum) 


datamatbm = datamatbm[which(rowSums(datamatbm[,8:27]) !=0),]

#matrix of biomass
matbm= as.matrix(datamatbm[,8:27])

#matrix of relative biomass
matbmRA = matbm/rowSums(matbm) 


a2 = adonis2(matbmRA ~ Treatment + Site+Type, data = datamatbm, na.rm = TRUE)
a2
write.csv(a2, "biofouling/dietzoop_permanovaBM.csv")


#Count version
datamatc = filter(Allbugsbm, Treatment != "Outside", `TaxaGroup` != "Empty", Treatment != "FMWT", ) %>%
  mutate(SampleID = paste(CageID, Treatment, Date, Site, FishID)) %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `TaxaGroup`, Count, SampleID) %>%
  pivot_wider(names_from = `TaxaGroup`, values_from = Count, values_fill = 0,
              values_fn = sum) 

datamatc = datamatc[which(rowSums(datamatc[,8:27]) !=0),]

matc= as.matrix(datamatc[,8:27])
matcRA = matc/rowSums(matc) 


a3 = adonis2(matcRA ~ Treatment + Site+Type, data = datamatc, na.rm = TRUE)
a3
write.csv(a3, "biofouling/dietzoop_permanova_count.csv")

##############################################################
#Permanova comparing zooplankton between treatments and with FMWT

datamatbmZ = filter(Allbugs2,`TaxaGroup` != "Empty", 
                   Taxa != "Amphipod",
                   Type %in% c("Exterior\nZooplankton", "Cage\nZooplankton")) %>%
  ungroup() %>%
  select(SampleID, Site,Treatment,  Type, TaxaGroup, Biomass) %>%
  pivot_wider(names_from = TaxaGroup, values_from = Biomass, values_fill = 0,
              values_fn = sum) 
#remove empty rows
datamatbmZ = datamatbmZ[which(rowSums(datamatbmZ[,5:16]) !=0),]

#matrix of biomass
matbmZ= as.matrix(datamatbmZ[,5:16])

#matrix of relative biomass
matbmRAZ = matbmZ/rowSums(matbmZ) 


a21 = adonis2(matbmRAZ ~ Treatment + Site, data = datamatbmZ, na.rm = TRUE)
a21
write.csv(a21, "biofouling/zoopwFMWT_permanovaBM.csv")

#individual PERMANOVAs comparing FMWT to cages, cage treatmetns to each other  
cagematbmZ = filter(datamatbmZ, Treatment %in% c("FMWT", "Outside"))

#matrix of biomass
matbmZ2= as.matrix(cagematbmZ[,5:16])

#matrix of relative biomass
matbmRAZ2 = matbmZ2/rowSums(matbmZ2) 


a2x = adonis2(matbmRAZ2 ~ Treatment + Site, data = cagematbmZ, na.rm = TRUE)
a2x
#OK, so FMWT is way different from the outside the cage samples

#now compare biofouling treatments
cagematbmZ2 = filter(datamatbmZ, Treatment %in% c("Exchanged", "Scrubbed"))

#matrix of biomass
matbmZ3= as.matrix(cagematbmZ2[,5:16])

#matrix of relative biomass
matbmRAZ3 = matbmZ3/rowSums(matbmZ3) 


a2xb = adonis2(matbmRAZ3 ~ Treatment + Site, data = cagematbmZ2, na.rm = TRUE)
a2xb

write.csv(a2xb, "biofouling/permanova_scrubvex_zoops.csv")
#No significant difference bewteen biofouling treatments
#now compare inside versus outside
cagematbmZ2b = filter(datamatbmZ, Treatment %in% c("Exchanged", "Scrubbed", "Outside"))

#matrix of biomass
matbmZ3b= as.matrix(cagematbmZ2b[,5:16])

#matrix of relative biomass
matbmRAZ3b = matbmZ3b/rowSums(matbmZ3b) 


a2xbb = adonis2(matbmRAZ3b ~ Type + Site, data = cagematbmZ2b, na.rm = TRUE)
a2xbb
#No difference!

write.csv(a2xbb, "biofouling/permanova_invout_zoops.csv")

#################################################################
#PERMANOVA on biofouling bugs
datamatamps = filter(Allbugs2,
                    Type %in% c("Biofouling\nMacroinvertebrates"),
                    TaxaGroup != "Other") %>%
  ungroup() %>%
  select(SampleID, Site,Treatment,  Type, TaxaGroup, Biomass) %>%
  pivot_wider(names_from = TaxaGroup, values_from = Biomass, values_fill = 0,
              values_fn = sum) 
#remove empty rows
#matrix of biomass
matam= as.matrix(datamatamps[,5:8])

#matrix of relative biomass
matamRA= matam/rowSums(matam) 


aAmp = adonis2(matamRA ~ Treatment + Site, data = datamatamps, na.rm = TRUE)
aAmp
write.csv(aAmp, "biofouling/Amphipods_permanovaBM.csv")

#################################################################
#PERMANOVA on just diet bugs
datamatd = filter(Allbugs2,
                     Type %in% c("Smelt Diet")) %>%
  ungroup() %>%
  select(SampleID, Site,Treatment,  Type, TaxaGroup, Biomass) %>%
  pivot_wider(names_from = TaxaGroup, values_from = Biomass, values_fill = 0,
              values_fn = sum) 
#remove empty rows
datamatd = datamatd[which(rowSums(datamatd[,5:17]) !=0),]


#matrix of biomass
matd= as.matrix(datamatd[,5:17])

#matrix of relative biomass
matdRA= matd/rowSums(matd) 


aD = adonis2(matdRA ~ Treatment + Site, data = datamatd, na.rm = TRUE)
aD
write.csv(aD, "biofouling/Diet_permanovaBM.csv")


  #############################################################
  
#Make the four panel plot of algal biomass, bug biomass, zoop biomass, and diet biomass

#total biomass and abundance
AllbugsTot = Allbugsbm %>%
  group_by(Site, Treatment, Type, CageID, SampleID) %>%
  summarize(Count = sum(Count), Biomass = sum(Biomass, na.rm =T))

#add algal biomass
algae<-read_csv("biofouling/Algae_biomass.csv")

#Convert biomass to milligrams to match the other measurements
algae = rename(algae, Site = Station, CageID = Cage) %>%
  mutate(Biomass = Dry_Weight*1000,Type = "Algae", CageID = as.character(CageID), SampleID = paste(CageID, Treatment, Date, Site)) %>%
  select(Site, Treatment, Type, CageID, Biomass, SampleID) %>%
  mutate(Site = case_when(Site == "Rio_Vista" ~ "Rio Vista",
                          TRUE ~ Site))

Allbugs_walgae = bind_rows(algae, AllbugsTot) %>%
  mutate(Biomass = case_when(Type == "Biofouling" ~ Biomass,
                             Type == "Zooplankton" ~ Biomass/1000,
                             TRUE ~ Biomass))

#replace diet biomass with diet fullness

#load(file = "data/GutFullness_cagecfull.RData")
load(file = "biofouling/calcfull.RData")

fullness = calcfullness %>%
  mutate( Type = "Diet", Biomass = `Percent Fullness (%BW)`,
          Site = case_when(Location ==  "Belden's Landing"~ "Montezuma",
                                                      TRUE ~ Location))


Reallyallbugs = bind_rows(fullness, filter(Allbugs_walgae, Type != "Diet")) %>%
  mutate(Treatment = factor(Treatment, levels = c("Exchanged", "Scrubbed", "Outside", "FMWT")),
         Type2 = factor(Type, levels = c("Algae", "Biofouling", "Zooplankton","Diet"),
                       labels = c("Algae (mg)", "Biofouling \nMacroinvertebrates (mg)", "Zooplankton (mg/m3)",
                                  "Gut Fullness (%)")),
         Site = factor(Site, levels = c("Rio Vista", "Montezuma")))

ggplot(Reallyallbugs, aes(x = Treatment, y = Biomass, fill = Site)) +
  geom_boxplot()+
  facet_wrap(~Type2, scales = "free")+
  theme_bw()+
  scale_fill_manual(values = c("#009E73",
                               "#D55E00"),
                    labels=c("Rio Vista", "Belden's Landing"))+
  theme(legend.position = "bottom")+
  ylab(NULL)+
  xlab(NULL)

ggsave("plots/FourPanelBiomass.tiff", device = "tiff", width =6, height =5)
##################################################################
#Statistics

#algae anova
alglm = lm(log(Biomass+1)~Site+Treatment, data = algae)
summary(alglm)
Anova(alglm)
#OK, pretty close to what Jesse found

#macroinvertanova
macs = filter(Reallyallbugs, Type == "Biofouling")

bfglm = lm(log(Biomass+1)~Site+Treatment, data = macs)
summary(bfglm)
Anova(bfglm)
#Biomass results is pretty similar to abundance results, don't need both
bfglm2 = lm(log(Count+1)~Site+Treatment, data = macs)
summary(bfglm2)
anova(bfglm2)
#yup, this is what Jesse got

#Zooplankton ANOVA
zoops = filter(Reallyallbugs, Type == "Zooplankton")
zglm2 = lm(log(Biomass+1)~Site+Treatment, data = zoops)
summary(zglm2)
Anova(zglm2)
plot(zglm2)
emmeans(zglm2, pairwise ~ Treatment)
emmeans(zglm2, pairwise ~ Site)
#no difference between treatments, but biomass is higher in FMWT

zglmx = lm(log(Count+1)~Site+Treatment, data = zoops)
summary(zglmx)
Anova(zglmx)
plot(zglmx)
emmeans(zglmx, pairwise ~ Treatment)
emmeans(zglmx, pairwise ~ Site)

diet = filter(Reallyallbugs, Type == "Diet")
dlm2 = lm(log(Biomass+1)~Site+Treatment, data = diet)
summary(dlm2)
Anova(dlm2)
plot(dlm2)
emmeans(dlm2, pairwise ~ Treatment)
emmeans(dlm2, pairwise ~ Site)


#########################################################
#ANOVAs of Limnoithona abundance, P forbesi abundance, and p nauplii abundance
#But I have to add the zeros in first!!

allwideCPUE = pivot_wider(allinterestingbugs, id_cols = c(SampleID, Date, Type, Treatment, Site),
                          names_from = Species, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = c(Calanoid_other:last_col()), names_to = "Species", values_to = "CPUE")

limno = filter(allwideCPUE, Species == "Limnoithona")
limlm = lm(log(CPUE+1) ~ Treatment + Site, data = limno)
summary(limlm)
Anova(limlm)
emmeans(limlm, pairwise ~ Treatment)

pfor = filter(allwideCPUE,
              Species == "Pseudodiaptomus forbesi")
pforlm = lm(log(CPUE+1) ~ Treatment + Site, data = pfor)
summary(pforlm)
Anova(pforlm)
emmeans(pforlm, pairwise ~ Treatment)
emmeans(pforlm, pairwise ~ Site)
plot(allEffects(pforlm))