#biofouling 2023 analyses

library(tidyverse)
library(lubridate)
library(readxl)

#look at the survival data
survival = read_excel("data/BiofoulingStudy_Survival.xlsx", sheet = "Data")

ggplot(survival, aes(x = Treatment, y = Survival))+ geom_boxplot()

ggplot(survival, aes(x = Site, y = Survival)) + geom_boxplot()

ggplot(survival, aes(x = Treatment, y = Survival, fill = Site)) + geom_boxplot()

#make a prettier version of the plot for the summer-fall habitat report
survival = mutate(survival, Treatment = factor(Treatment, levels = c("Flip", "Scrub"), 
                                               labels = c("Exchanged", "Scrubbed")),
                  Site = factor(Site, levels = c("RV", "SM"), labels = c("Rio Vista", "Belden's Landing")))


ggplot(survival, aes(x = Treatment, y = Survival, fill = Site)) + geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")

ggplot(survival, aes(x = Cage, y = Survival, fill = Site)) + geom_col()+
  theme_bw()+
  facet_wrap(~Treatment, scales = "free_x")+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")


#Test for statistically significant differences
lm1 = lm(CountwithCTM ~ Treatment+Site, data = survival)
summary(lm1)
anova(lm1)
plot(lm1)

ggplot(survival, aes(x = Cage, y = Survival, fill = Treatment))+ geom_col()+
  facet_wrap(~Site, scales = "free_x")
hist(survival$CountwithCTM)

#quick plots of environmental data

env = read_excel("data/BiofoulingStudyCageChecks.xlsx", na = c("NA", ""))

ggplot(env, aes(x = as.factor(Date), y = DO_mg_L, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)
  
ggplot(env, aes(x = as.factor(Date), y = pH, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)

library(lme4)
test = lmer(DO_mg_L ~ InsideOutside + (1|Date), data = env)
summary(test)

ggplot(env, aes(x = as.factor(Date), y = Water.Temp_C, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)


ggplot(env, aes(x = as.factor(Date), y = Turb, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)


BDL = cdec_query("BDL", c(25, 27, 100, 62, 221), start.date = ymd("2023-08-30"),
                 end.date = ymd("2023-09-01"))
BDL2 = BDL %>%
  filter(date(DateTime) == ymd("2023-08-31"), Duration == "E") %>%
  mutate(Value2 = case_when(SensorType == "TEMP W" ~ (Value-32)*5/9,
                            TRUE ~ Value))
