#clean copy of all the plots going in teh biofouling manuscript

library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(emmeans)
library(car)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white", "orangered", "black")

load("biofouling/Allbugs.Rdata")
load("biofouling/Allbugsbm.Rdata")



#Plots
#######################################################

#This is the figure for the paper with relative abundance and relative biomass
RBM = ggplot(Allbugsbm , aes(x = Treatment, y = Biomass, fill = `Taxa Group`)) + 
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

RA = ggplot(Allbugsbm , aes(x = Treatment, y = Count, fill = `Taxa Group`)) + 
  geom_col(position = "fill")+
  facet_grid(Type~Site)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  theme_bw()

RBM+RA

ggsave("plots/dietzoopamp_relativeabundance.tiff", width =10, height =6, device = "tiff")


##########################################################
#now the absolute abundance

AllbugsSum = group_by(Allbugsbm, `Taxa Group`, Site, Treatment, Type) %>%
  summarize(Count = mean(Count), Biomass = mean(Biomass))

TAB = ggplot(filter(AllbugsSum, `Taxa Group` != "Empty"), aes(x = Treatment, y = Count, fill = `Taxa Group`)) +
  facet_grid(Type ~ Site, scale = "free_y")+
  geom_col()+
  scale_fill_manual(values = mypal)+
  ylab("Mean Number per Sample")+
  theme_bw()
  
  
TBM =   ggplot(filter(AllbugsSum, `Taxa Group` != "Empty"), 
               aes(x = Treatment, y = Biomass, fill = `Taxa Group`)) +
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
allinbug = rename(allinterestingbugs, CageID = CageNum, Count = CPUE,
                  `Taxa Group` = Species, Biomass = Mass) %>%
  filter(Treatment != "Flip", Treatment != "Scrub") %>%
  mutate(Type = case_when(Treatment %in% c("FMWT", "Outside") ~"Exterior\nZooplankton",
                          TRUE ~ Type))

Allbugs2 = bind_rows(Allbugsbm, allinbug) %>%
  filter(`Taxa Group` != "Empty") %>%
  mutate(`Taxa Group` = case_when(`Taxa Group` == "Calanoid_other" ~ "Calanoid Copepod",
                                  `Taxa Group` == "Cyclopoid_other" ~ "Cyclopoid Copepod",
                                  `Taxa Group` %in% c("Pseudodiaptomus", "Pseudodiaptomus forbesi", 
                                  "Pseudodiaptomus Juvenile") ~ "Pseudodiaptomus",
                                  `Taxa Group` == "Pseudodiaptomus nauplii" ~ "Copepod nauplii",
                                  `Taxa Group` == "Harpaticoida" ~ "Harpacticoid",
                                  `Taxa Group` == "Harpaticoid" ~ "Harpacticoid",
                                  `Taxa Group` == "Cladocera" ~ "Cladoceran",
                                  TRUE ~ `Taxa Group`)) %>%
  mutate(Type = factor(Type, levels = c("Diet", "Biofouling", "Zooplankton", "Exterior\nZooplankton"),
                       labels =  c("Smelt Diet", "Biofouling\nMacroinvertebrates", "Cage\nZooplankton", "Exterior\nZooplankton"))) %>%
  mutate(Taxa = factor(`Taxa Group`, levels = c("Calanoid Copepod", "Eurytemora affinis","Pseudodiaptomus",
                                                "Cyclopoid Copepod","Limnoithona","Copepod nauplii", "Harpacticoid",
                                                "Cladoceran","Rotifer", "Amphipod", "Isopod", "Cumacean" ,
                                                "Insect", "Chironomid", "Gastropod",
                                                "Ostracod", "Tanaid", "Other")))

RBM2 = ggplot(Allbugs2 , aes(x = Treatment, y = Biomass, fill = Taxa)) + 
  geom_col(position = "fill")+
  facet_grid(Site ~ Type, scales = "free_x")+
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
  facet_grid(Site ~ Type, scales = "free_x")+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  xlab(NULL)+
  theme_bw()+
  theme(legend.position = "bottom")

RA2

RBM2/RA2



ggsave("plots/dietzoopamp_barchart_wfmwt.tiff", width =10, height =10, device = "tiff")


################################################
#Now the statistics

#permanova comparing the three data type, two  sites, and two treatments
datamatbm = filter(Allbugsbm, Treatment != "Outside", Treatment != "FMWT", `Taxa Group` != "Empty") %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `Taxa Group`, Biomass) %>%
  pivot_wider(names_from = `Taxa Group`, values_from = Biomass, values_fill = 0,
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

##############################################################
#Permanova comparing zooplankton between treatments and with FMWT

#permanova comparing the three data type, two  sites, and two treatments
datamatbmZ = filter(Allbugs2,`Taxa Group` != "Empty", 
                   Taxa != "Amphipod",
                   Type %in% c("Exterior\nZooplankton", "Cage\nZooplankton")) %>%
  select(Site,Treatment, CageID, SampleID, Date, Type, Taxa, Biomass) %>%
  pivot_wider(names_from = Taxa, values_from = Biomass, values_fill = 0,
              values_fn = sum) 
#remove empty rows
datamatbmZ = datamatbmZ[which(rowSums(datamatbmZ[,7:21]) !=0),]

#matrix of biomass
matbmZ= as.matrix(datamatbmZ[,7:21])

#matrix of relative biomass
matbmRAZ = matbmZ/rowSums(matbmZ) 


a2 = adonis2(matbmRAZ ~ Treatment + Site, data = datamatbmZ, na.rm = TRUE)
a2
write.csv(a2, "biofouling/zoopwFMWT_permanovaBM.csv")

#individual PERMANOVAs comparing FMWT to cages, cage treatmetns to each other  
cagematbmZ = filter(datamatbmZ, Treatment %in% c("FMWT", "Outside"))

#matrix of biomass
matbmZ2= as.matrix(cagematbmZ[,7:21])

#matrix of relative biomass
matbmRAZ2 = matbmZ2/rowSums(matbmZ2) 


a2x = adonis2(matbmRAZ2 ~ Treatment + Site, data = cagematbmZ, na.rm = TRUE)
a2x
#OK, so FMWT is way different from the outside the cage samples

#now compare biofouling treatments
cagematbmZ2 = filter(datamatbmZ, Treatment %in% c("Exchanged", "Scrubbed"))

#matrix of biomass
matbmZ3= as.matrix(cagematbmZ2[,7:21])

#matrix of relative biomass
matbmRAZ3 = matbmZ3/rowSums(matbmZ3) 


a2xb = adonis2(matbmRAZ3 ~ Treatment + Site, data = cagematbmZ2, na.rm = TRUE)
a2xb
#No significant difference bewteen biofouling treatments
#now compare inside versus outside
cagematbmZ2b = filter(datamatbmZ, Treatment %in% c("Exchanged", "Scrubbed", "Outside"))

#matrix of biomass
matbmZ3b= as.matrix(cagematbmZ2b[,7:21])

#matrix of relative biomass
matbmRAZ3b = matbmZ3b/rowSums(matbmZ3b) 


a2xbb = adonis2(matbmRAZ3b ~ Treatment + Site, data = cagematbmZ2b, na.rm = TRUE)
a2xbb
#There is a significant difference bewteen inside and outside, though R2 is only0.12
#This is maybe different than what Scott found?


  #############################################################
  
#Make the four panel plot of algal biomass, bug biomass, zoop biomass, and diet biomass

#total biomass and abundance
AllbugsTot = mutate(Allbugsbm, SampleID = paste(CageID, Treatment, Date, Site, FishID)) %>%
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

Allbugs_walgae = bind_rows(algae, AllbugsTot)

#exterior zooplankton samples
outbugs = allinterestingbugs %>%
  mutate( Type = "Zooplankton", Biomass = Mass)

outbugsum = group_by(outbugs, Site, Date, Treatment, Type, SampleID) %>%
  summarize(Biomass = sum(Biomass, na.rm =T), CPUE = sum(CPUE)) %>%
  filter(Treatment != "Scrub", Treatment != "Flip")

Reallyallbugs = bind_rows(outbugsum, Allbugs_walgae) %>%
  mutate(Treatment = factor(Treatment, levels = c("Exchanged", "Scrubbed", "Outside", "FMWT")))

ggplot(Reallyallbugs, aes(x = Treatment, y = Biomass, fill = Site)) +
  geom_boxplot()+
  facet_wrap(~Type, scales = "free")+
  theme_bw()+
  scale_fill_manual(values = c("#009E73",
                               "#D55E00"),
                    labels=c("Belden's Landing", "Rio Vista"))+
  theme(legend.position = "bottom")

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

zoops = mutate(zoops, CPUE = case_when(is.na(CPUE) ~ Count,
                                       TRUE ~ CPUE))
zglmx = lm(log(CPUE+1)~Site+Treatment, data = zoops)
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