#permanova code demo for Christina
#2024-09-12
#Rosemary Hartman

library(vegan)
library(tidyverse)

#load teh community information in long format
load("biofouling/Allbugsbm.RData")

#switch to wide format
datamatbm = filter(Allbugsbm, Treatment != "Outside", `Taxa Group` != "Empty") %>%
  select(Site,Treatment, CageID, FishID, Date, Type, `Taxa Group`, Biomass) %>%
  pivot_wider(names_from = `Taxa Group`, values_from = Biomass, values_fill = 0) 

#make sure it looks right
View(datamatbm)

#remove any rows containing zero catch
datamatbm = datamatbm[which(rowSums(datamatbm[,7:22]) !=0),]

#matrix of biomass
matbm= as.matrix(datamatbm[,7:22])

#matrix of relative biomass
matbmRA = matbm/rowSums(matbm) 

#use the relative abundance matrix as your resposne variable, other factors as predicors
a2 = adonis2(matbmRA ~ Treatment + Site+Type, data = datamatbm, na.rm = TRUE)
a2

#Be sure and look at the R2 values too. 

##############################################
#

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
