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

clods2a = clods2 %>%
  rename(InsideOutside = treatment, Starting_Weight = `Starting Weight`, Ending_Weight = `Ending weight`,
         StartDate = `start date`,
         EndDate = 'end date') %>%
  mutate(Cage = paste("Cage", CageNum), Treatment = factor(treatment2, levels = c("outside", "flip", "scrub"),
                                            labels = c("External", "Exchanged", "Scrubbed")),
                                            Site = factor(Site, levels = c( "RV","SM"),
                                                                labels = c( "Rio Vista","Suisun Marsh")),
                days = case_when(Site == "Rio Vista" ~ 7,
                                 Site == "Suisun Marsh" ~ 6),
                DiffPerDay = difference/days) %>%
  select(Site, Cage, InsideOutside, Treatment, StartDate, Starting_Weight, EndDate, Ending_Weight, difference, DiffPerDay)

write.csv(clods2a, "data/clodcards.csv", row.names = F)


ggplot(clods2a, aes(x = Site, y = difference, fill = treatment2))+ geom_boxplot()
ggplot(clods2a, aes(x = Site, y = DiffPerDay, fill = treatment2))+ geom_boxplot()

ggplot(clods2a, aes(x = Treatment, y = DiffPerDay, fill = Site)) +
  geom_boxplot()+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+ 
  ylab("Loss of Weight per Day (g)")+xlab(NULL)+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("plots/clodcards.tiff", device = "tiff", width =4, height =5)

ggplot(clods2a, aes(x = InsideOutside, y = DiffPerDay, fill = Site)) +
  geom_boxplot()+
  scale_fill_manual(values = c( "#FEE08b","#f46d43"), labels = c("RV", "BDL"))+ 
  ylab("Loss of Weight per Day (g)")+xlab(NULL)+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("plots/clodcards.png", device = "png", width =4, height =4)

clodlm = lm(DiffPerDay ~ Site+treatment2, data = clods2)
summary(clodlm)
anova(clodlm)
plot(clodlm)
hist(residuals(clodlm))
pairs(emmeans(clodlm, ~treatment2))
summary(aov(clodlm))
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
