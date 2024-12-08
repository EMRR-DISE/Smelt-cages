
#################################################################
#growht data
library(tidyverse)
library(lubridate)
library(readxl)
library(RColorBrewer)
growth = read_xlsx("data/BiofoulingStudyCageRemovals.xlsx")


str(growth)
hist(growth$Weight)
hist(growth$ForkLength)
hist(log(growth$Weight))

#CF = Wb/L^3 * 100 where wb is body weight in mg and L is fork length in mm


growth = mutate(growth, Treatment =factor(Treatment, levels = c("Control", "Flip", "Scrub"), 
                                           labels = c("Control", "Exchanged", "Scrubbed")),
                Site = factor(Location, levels = c( "Montezuma", "FCCL","Rio Vista"), 
                              labels = c( "Belden's Landing","FCCL","Rio Vista" )),
                Treatment = case_when(is.na(Treatment) ~ "Control",
                                      TRUE ~ Treatment),
                Condition = Weight*1000/(ForkLength^3)*100)

ggplot(growth, aes(x = Treatment, fill = Site, y = ForkLength))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Fork Length (mm)")+
  coord_cartesian(ylim = c(35,75))

ggplot(growth, aes(x = Treatment, fill = Site, y = Weight))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Weight (g)")

growth = mutate(growth, Site = factor(Site, levels = c("FCCL", "Belden's Landing", "Rio Vista")))
brewer.pal(3, "Dark2")

ggplot(growth, aes(x = Treatment, fill = Site, y = Weight))+ geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values = c( "#D95F02","#1B9E77", "#7570B3"))+
  theme(legend.position = "bottom")+
  ylab("Weight (g)")


ggplot(growth, aes(x = Treatment, fill = Site, y = Condition))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Condition Factor")


g1 = lm(Weight ~ Location + Treatment, data = growth)
summary(g1)
plot(g1)

g2 = lm(ForkLength ~ Location + Treatment, data = growth)
summary(g2)
plot(g2)


g2 = lm(Condition~ Location + Treatment, data = growth)
summary(g2)
plot(g2)

#######################################################################
#now for lab data

labgrowth = read_excel("data/BiofoulingStudy_FishDissections.xlsx")


#CF = Wb/L^3 * 100 where wb is body weight in mg and L is fork length in mm


labgrowth = mutate(labgrowth, Treatment = factor(Treatment, levels = c("Control", "Flip", "Scrub"), 
                                           labels = c("Control", "Exchanged", "Scrubbed")),
                Site = factor(Location, levels = c( "Montezuma", "FCCL","Rio Vista"), 
                              labels = c( "Belden's Landing","FCCL","Rio Vista" )),
                Condition = LabWeight_g*1000/(LabForkLength_mm^3)*100)

ggplot(labgrowth, aes(x = Treatment, fill = Site, y = LabForkLength_mm))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Fork Length (mm)")+
  
  coord_cartesian(ylim = c(35,75))

ggplot(labgrowth, aes(x = Treatment, fill = Site, y = LabWeight_g))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Weight (g)")

ggplot(labgrowth, aes(x = Treatment, fill = Site, y = Condition))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Condition Factor")


ggsave("plots/ConditionFactor.tiff", device = "tiff", width =6, height =5)

g1 = lm(LabWeight_g ~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g1)
plot(g1)

g2 = lm(ForkLength ~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g2)
plot(g2)


g2 = lm(Condition~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g2)
plot(g2)

#############################################
allgrowth = left_join(growth, labgrowth, by = "FishID")

ggplot(allgrowth, aes(x = Weight, y = LabWeight_g)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

ggplot(allgrowth, aes(x = ForkLength, y = LabForkLength_mm)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)
