#clean copy of all the plots going in teh biofouling manuscript

library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"), "yellow", "orange", "purple", "grey", "brown",
          "red", "lightgreen", "white", "orangered", "black")

load("biofouling/Allbugs.Rdata")
load("biofouling/Allbugsbm.Rdata")



#Plots
#######################################################
lab = data.frame(Type = "Biofouling", Biomass = 0.9, Site = "Montezuma", Treatment = "Exchanged", label = "A")
lab2 = data.frame(Type = "Biofouling", Biomass = 0.9, Site = "Montezuma", Treatment = "Exchanged", label = "B")


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

RA = ggplot(Allbugsbm , aes(x = Treatment, y = Count, fill = `Taxa Group`)) + geom_col(position = "fill")+
  facet_grid(Type~Site)+
  scale_fill_manual(values = mypal)+
  ylab("Relative Abundance")+
  theme_bw()#+
  #geom_text(data = lab2, aes(x = Type, y = Biomass, label = label), 
  #          size =8, inherit.aes = FALSE) +
  #xlab(NULL)

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
  
  
TBM =   ggplot(filter(AllbugsSum, `Taxa Group` != "Empty"), aes(x = Treatment, y = Biomass, fill = `Taxa Group`)) +
    facet_grid(Type ~ Site, scale = "free_y")+
    geom_col()+
    scale_fill_manual(values = mypal)+
    ylab("Mean Biomass (ug) per Sample")+
  theme_bw()+
  theme(legend.position = "none")

TBM+TAB


ggsave("plots/dietzoopamp_absoluteeabundance.tiff", width =10, height =6, device = "tiff")


################################################
#Now the statistics

#permanova comparing the three data type, two  sites, and two treatments
datamatbm = filter(Allbugsbm, Treatment != "Outside", `Taxa Group` != "Empty") %>%
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
  scale_fill_manual(values = mypal)
