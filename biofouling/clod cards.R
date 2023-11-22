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

clods2 = mutate(clods2, treatment2 = factor(treatment2, levels = c("outside", "flip", "scrub")),
                days = case_when(Site == "RV" ~ 7,
                                 Site == "SM" ~ 6),
                DiffPerDay = difference/days)


ggplot(clods2, aes(x = Site, y = difference, fill = treatment2))+ geom_boxplot()
ggplot(clods2, aes(x = Site, y = DiffPerDay, fill = treatment2))+ geom_boxplot()

clodlm = lm(DiffPerDay ~ Site+treatment2, data = clods2)
summary(clodlm)
pairs(emmeans(clodlm, ~treatment2))

ggplot(clods2, aes(x = Site, y = percentdiff, fill = treatment2))+ geom_boxplot()

clodlm = lm(percentdiff ~ Site+treatment2, data = clods2)
summary(clodlm)
pairs(emmeans(clodlm, ~treatment2))
