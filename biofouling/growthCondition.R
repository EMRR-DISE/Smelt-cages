
#################################################################
#growht data
library(tidyverse)
library(lubridate)
library(readxl)
growth = read_xlsx("data/BiofoulingStudyCageRemovals.xlsx")


str(growth)
hist(growth$Weight)
hist(growth$ForkLength)
hist(log(growth$Weight))

#CF = Wb/L^3 * 100 where wb is body weight in mg and L is fork length in mm


growth = mutate(growth, Treatment = factor(Treatment, levels = c("Flip", "Scrub"), 
                                           labels = c("Exchanged", "Scrubbed")),
                Site = factor(Location, levels = c("Rio Vista", "Montezuma", "FCCL"), 
                              labels = c("Rio Vista", "Belden's Landing", "FCCL")),
                Condition = Weight*1000/(ForkLength^3)*100)

ggplot(growth, aes(x = Treatment, fill = Site, y = ForkLength))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Fork Length (mm)")

ggplot(growth, aes(x = Treatment, fill = Site, y = Weight))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
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
                Site = factor(Location, levels = c("Rio Vista", "Montezuma", "FCCL"), 
                              labels = c("Rio Vista", "Belden's Landing", "FCCL")),
                Condition = LabWeight_g*1000/(LabForkLength_mm^3)*100)

ggplot(labgrowth, aes(x = Treatment, fill = Site, y = LabForkLength_mm))+ geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  ylab("Fork Length (mm)")

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


g1 = lm(LabWeight_g ~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g1)
plot(g1)

g2 = lm(ForkLength ~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g2)
plot(g2)


g2 = lm(Condition~ Location + Treatment, data = filter(labgrowth, Location != "FCCL"))
summary(g2)
plot(g2)

