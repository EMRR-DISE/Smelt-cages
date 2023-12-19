#graph test clod card data (because I can!)

library(tidyverse)
library(lubridate)
library(readxl)
library(emmeans)

clods = read_excel("data/clod cards.xlsx")

clods = mutate(clods, diff = `Starting Weight`-`Ending weight`) %>%
  filter(treatment != "not used")

ggplot(clods, aes(x = treatment, y = diff))+ geom_boxplot()
ggplot(clods, aes(x = treatment, y = diff, fill =as.factor(`Card #`)))+ 
  geom_col(position = "dodge", color = "black")+
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  ylab("Difference in Weight (g)")+
  theme_bw()


clods2 = read_excel("data/clod cards.xlsx", sheet = "finalcards") %>%
  filter(!is.na(treatment)) 

clods2 = mutate(clods2, treatment2 = factor(treatment2, levels = c("outside", "flip", "scrub"),
                                            labels = c("External", "Exchanged", "Scrubbed")),
                                            Site = factor(Site, levels = c("RV", "SM"),
                                                                labels = c("Rio Vista", "Belden's Landing")),
                days = case_when(Site == "Rio Vista" ~ 7,
                                 Site == "Belden's Landing" ~ 6),
                DiffPerDay = difference/days)


ggplot(clods2, aes(x = Site, y = difference, fill = treatment2))+ geom_boxplot()
ggplot(clods2, aes(x = Site, y = DiffPerDay, fill = treatment2))+ geom_boxplot()

ggplot(clods2, aes(x = treatment2, y = DiffPerDay, fill = Site)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw() + ylab("Loss of Weight per Day (g)")+xlab(NULL)

ggsave("plots/clodcards.tiff", device = "tiff", width =6, height =5)

clodlm = lm(DiffPerDay ~ Site+treatment2, data = clods2)
summary(clodlm)
pairs(emmeans(clodlm, ~treatment2))

ggplot(clods2, aes(x = Site, y = percentdiff, fill = treatment2))+ geom_boxplot()

clodlm = lm(percentdiff ~ Site+treatment2, data = clods2)
summary(clodlm)
pairs(emmeans(clodlm, ~treatment2))


clodlm = lm(percentdiff ~ Site+treatment2, data = filter(clods2, treatment2 != "outside"))
summary(clodlm)
pairs(emmeans(clodlm, ~treatment2))

library(cder)

BDLRVB = cdec_query(stations = c("NSL", "SRV"), sensors = 21, start.date = as.Date("2023-08-29"), end.date = as.Date("2023-10-15"))

ggplot(BDLRVB, aes(x = DateTime, y = Value, color = StationID)) + geom_line(size =1)+ ylab("velocity")
