#2024 zoop and amphipod data
library(tidyverse)
library(readxl)


library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set3"), "black")


zoops = read_excel("smelt_2024_SMSCGs/DWR_CageZoop_2024_Complete_TEC_12.10.24.xlsx", sheet = "Zoop Data")
#Crosswalk
crosswalk = read_csv("data/Cage Biomass crosswalk Updated.csv")

#correct for subsampling and plot

zoops = mutate(zoops, TotCount = Count*`Sample Volume (mL)`/`# of Subsamples`,
               InOut = case_when(str_detect(Location, "Outside") ~ "Out",
                                 TRUE ~ "IN"))


ggplot(zoops, aes(x = as.factor(Date), y = TotCount, fill = `Species Name`))+ geom_col()+
  facet_wrap(InOut~Site)


ggplot(filter(zoops, `Species Name` %in% c("Pseudodiaptomus forbesi adult", "Pseudodiaptomus forbesi copepodite",
                                           "Pseudodiaptomus spp nauplii")),
       aes(x = as.factor(Date), y = TotCount, fill = `Species Name`))+ geom_col()+
  facet_wrap(~Site)

ggplot(filter(zoops, `Species Name` %in% c("Limnoithona tetraspina adult", "Limnoithona spp copepodite",
                                           "Limnoithona sinensis adult")),
       aes(x = as.factor(Date), y = TotCount, fill = `Species Name`))+ geom_col()+
  facet_wrap(InOut~Site)

#So, higher total CPUE at Montezuma again, but mostly nauplii and limnos. Higher psudos at rio vista

amphs = read_excel("smelt_2024_SMSCGs/DWR_CageZoop_2024_Complete_TEC_12.10.24.xlsx", sheet = "Amph Data")

ggplot(amphs, aes(x = Location, y = total_count, fill = Species))+ geom_col()+
  scale_fill_manual(values = mypal)+
  theme_bw()

#Try adding the crosswalk and make sure it works

zoops2 = left_join(zoops, crosswalk, by = c("Species Name" = "CageCode")) %>%
  mutate(SampleID = paste(Location, Site, Date))

ggplot(zoops2, aes(x = Location, y = TotCount, fill = TaxaGroup))+ geom_col()+
  facet_wrap(InOut~Date, scales = "free_x")+
  scale_fill_manual(values = mypal)

ggplot(zoops2, aes(x = SampleID, y = TotCount, fill = TaxaGroup))+ geom_col()+
  facet_wrap(InOut~Site, scales = "free_x")+
  scale_fill_manual(values = mypal)

amphs2 = amphs %>%
  mutate(Species = str_remove(Species, " UNID")) %>%
  left_join(crosswalk, by = c("Species" = "CageCode") )
  
test = filter(amphs2, is.na(TaxaGroup))
unique(test$Species)

ggplot(amphs2, aes(x = Location, y = total_count, fill = TaxaGroup))+ 
  geom_col()+
theme_bw()+
    scale_fill_manual(values = mypal)
