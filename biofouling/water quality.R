#biofouling 2023 analyses

library(tidyverse)
library(lubridate)
library(readxl)
library(cder)
library(lme4)
library(lmerTest)
library(broom)

#look at the survival data
survival = read_excel("data/BiofoulingStudy_Survival.xlsx", sheet = "Data")

FCCl = data.frame(Treatment = "Control", Site = "FCCL", Cage = "FCCL", Survival = 21/24)

survival = bind_rows(survival, FCCl)

ggplot(survival, aes(x = Treatment, y = Survival))+ geom_boxplot()

ggplot(survival, aes(x = Site, y = Survival)) + geom_boxplot()

ggplot(survival, aes(x = Treatment, y = Survival, fill = Site)) + geom_boxplot()

#make a prettier version of the plot for the summer-fall habitat report
survival = mutate(survival, Treatment = factor(Treatment, levels = c("Control", "Flip", "Scrub"), 
                                               labels = c("Control", "Exchanged", "Scrubbed")),
                  Site = factor(Site, levels = c("SM","FCCL","RV"), labels = c("Belden's Landing","FCCL","Rio Vista")))


ggplot(survival, aes(x = Treatment, y = Survival, fill = Site)) + geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")

ggplot(survival, aes(x = Cage, y = Survival, fill = Site)) + geom_col()+
  theme_bw()+
  facet_wrap(~Treatment, scales = "free_x")+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")+
  coord_cartesian(ylim = c(0,1))


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

#######comparisons of inside and outside ###############################
DOmod = lmer(DO_mg_L ~ InsideOutside + (1|Date), data = env)
summary(DOmod)

tempmod = lmer(Water.Temp_C ~ InsideOutside + (1|Date), data = env)
summary(tempmod)

turbmod = lmer(Turb ~ InsideOutside + (1|Date), data = env)
summary(turbmod)

pHmod = lmer(pH ~ InsideOutside + (1|Date), data = env)
summary(pHmod)

#put all the summaries together and export
results = 

ggplot(env, aes(x = as.factor(Date), y = Water.Temp_C, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)


ggplot(env, aes(x = as.factor(Date), y = Turb, color = InsideOutside))+
  geom_boxplot()+
  facet_wrap(~Location)


BDL = cdec_query(c("BDL","RVB"), c(25, 27,28, 62, 100, 221), start.date = ymd("2023-08-30"),
                 end.date = ymd("2023-10-17"))
library(wql)
BDL2 = BDL %>%
  mutate(Value2 = case_when(SensorType == "TEMP W" ~ (Value-32)*5/9,
                            SensorType == "EL COND" ~ ec2pss(Value/1000, 25),
                            TRUE ~ Value),
         Analyte = factor(SensorType, levels = c("CHLORPH", "EL COND", "PH VAL",
                                                 "TEMP W", "TURB W"),
                          labels = c("Chlorophyll (ug/L)", "Salinity (PSU)",
                                     "pH", "Temperature (C)", "Turbidity (FNU)"))) %>%
  filter(Value2 <200, !(SensorType == "CHLORPH" & Value > 10)) %>%
  mutate(Site = factor(StationID, levels = c("RVB", "BDL")))

ggplot(filter(BDL2, SensorType != "PH VAL"), aes(x = DateTime, y = Value2, color = Site))+ geom_line()+
  facet_wrap(~Analyte, scales = "free", nrow =3)+
  scale_color_manual(values = c("#1B9E77","#D95F02"), name = "Site")+
  theme_bw()+ ylab(NULL)+
  xlab("Date - 2024")+
  theme(legend.position = "bottom")

ggsave("plots/continuousWQ_biofouling.tiff", device = "tiff", width =8, height =6)
