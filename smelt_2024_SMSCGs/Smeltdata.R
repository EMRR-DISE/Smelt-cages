#get means on teh pre-deployment fish

library(readxl)
library(tidyverse)
library(emmeans)
library(effects)

prefish = read_excel("Data/SMSCGStudyPredeploymentMeasurements.xlsx")

str(prefish)

summary(prefish$ForkLength)

fish = read_excel("Data/SMSCGStudyCageRemovals.xlsx")
str(fish)

#conidition factor K = 100*(Weight (g) /(Length (mm) *0.1)^3)
fish = mutate(fish, weight = as.numeric(Weight),
              Condition = 100*(weight/(ForkLength*0.1)^3))

ggplot(fish, aes(x = Location, y = ForkLength))+ geom_boxplot()
ggplot(fish, aes(x = Location, y = weight))+ geom_boxplot()

ggplot(fish, aes(x = Location, y = Condition, fill = Location))+ 
  geom_boxplot()+
  ylab("Condition Factor")+
  scale_fill_manual(values = c("orange", "tomato", "skyblue"), guide = NULL)+
  theme_bw()
hist(fish$Condition)

survival = read_excel("data/survival_2024.xlsx")

ggplot(survival, aes(x = Cage, y = `Survival%`, fill = Site))+
  geom_col()+
  scale_fill_manual(values = c("orange", "tomato", "skyblue"))+
  theme_bw()


conlm = lm(Condition ~ Location, data = fish)
summary(conlm)
plot(conlm)
#qq plot doesn't look great
conlm2 = lm(log(Condition) ~ Location, data = fish)
summary(conlm2)
plot(conlm2)
#log transformed is honestly not much better
Anova(conlm)
emmeans(conlm, pairwise ~ Location)
#montezuma is significantly lower than rio vista, both field sites are lower than FCCL
plot(allEffects(conlm))
plot(emmeans(conlm, pairwise ~ Location), comparison = T)

#check lab versus field fish
labfish = read_excel("Data/SmeltLabData2024.xlsx") %>%
  rename(LabFL = ForkLength, LabWeight = Weight)

fish2 = left_join(fish, labfish, by = "FishID") %>%
  mutate(WeightDiff = weight -LabWeight, LenDiff = ForkLength-LabFL,
         LabCondition = 100*(LabWeight/(LabFL*0.1)^3))

ggplot(fish2, aes(x = Condition, y = LabCondition)) + geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept =0)

ggplot(fish2, aes(x = ForkLength, y = LabFL)) + geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept =0)

ggplot(fish2, aes(x = weight, y = LabWeight)) + geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept =0)

problems = filter(fish2, LenDiff >2 | LenDiff < -2)

ggplot(fish2, aes(x = ForkLength, y = weight)) + geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept =0)

#Now HSI!

labfish = mutate(labfish, HSI = LiverWeight/LabWeight*100)

ggplot(labfish, aes(x = Location, y = HSI, fill = Location))+
  geom_boxplot()+
  scale_fill_manual(values = c("orange", "tomato", "skyblue"), guide = NULL)+
  theme_bw()

