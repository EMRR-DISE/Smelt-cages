library(tidyverse)
library(lubridate)
library(scales)
library(knitr)
library(mgcv)
library(lme4)
library(car)
library(emmeans)
library(gratia)
library(here)
library(forcats)
library(vegan)
library(cowplot)
#######ALGAE BIOMASS######
algae<-read.csv("biofouling/Algae_biomass.csv", stringsAsFactors = FALSE)
algae$Station <- factor(algae$Station)

algin <- algae %>% filter(Sample_Location=="Inside")
algin$logweight <- log(algin$Dry_Weight*100) 
algbox <- algin %>% group_by(Station,Treatment) %>% 
  summarise(biomass=sum(logweight))

fig9 <- algin %>% 
  ggplot(aes(x = Treatment, y = logweight, fill = Station)) +
  geom_boxplot()
figA <- fig9+
  labs(x = NULL, 
       y = bquote('Algal Biomass: [Log] Dry Weight (g) *100'), 
       title = NULL) +  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank())+
  theme(axis.ticks = element_blank())+
  scale_fill_manual(values = c("#009E73",
                               "#D55E00"),
                    labels=c("Belden's Landing", "Rio Vista"))
figA
leveneTest(algin$logweight ~ algin$Treatment)#homogeneity of variances assumption met (p=0.67)
res_aov9 <- aov(logweight ~ Treatment, data = algin)
hist(res_aov9$residuals)
qqPlot(res_aov9$residuals, id = FALSE)
shapiro.test(res_aov9$residuals)#not normally distributed (p=0.02); use linear model and anova for better fit

###### linear model and anova ##### 
modalgin <- lm(logweight ~ Treatment + Station,data = algin) 
summary(modalgin)
plot(modalgin)
shapiro.test(residuals(modalgin))#residuals of lm are normally distributed
modalgova <- Anova(modalgin, type = 2, test.statistic = "F") 
kable(modalgova)
summary(modalgova)
AIC(modalgin)

algin1 <- algin
modalgin1 <- lm(logweight ~ Treatment,data = algin1) 
summary(modalgin1)
AIC(modalgin1)

algin2 <- algin %>% filter(Station=='Rio_Vista')
modalgin2 <- lm(logweight ~ Treatment,data = algin2) 
summary(modalgin2)
AIC(modalgin2)

algin3 <- algin %>% filter(Station=='Montezuma')
modalgin3 <- lm(logweight ~ Treatment,data = algin3) 
summary(modalgin3)
AIC(modalgin3)

AIC(modalgin,modalgin1, modalgin2, modalgin3)
#########MACROINVERTEBRATES#######

amph<-read.csv("amphipods.csv", stringsAsFactors = FALSE)
amph$Site <- factor(amph$Site)
amph$Treatment <- factor(amph$Treatment)
amph$total_count <- as.numeric(amph$total_count)

#remove a couple of incidental pelagic taxa probably not associated with biofouling
amph <- amph %>% filter(Species!="Crab Zoea")
amph <- amph %>% filter(Species!="Ostracod")
amph2 <- amph %>% group_by(Site, Treatment, Location) %>% 
  summarise(count=sum(total_count))
fig3 <- amph2 %>% 
  ggplot(aes(x = Treatment, y = log((count+1)*16), fill = Site)) + # x16 to convert to m2
  geom_boxplot()
figB <- fig3+
  labs(x = NULL, 
       y = bquote('[Log] Macroinvertebrates *'~ m^-2), 
       title = NULL) +  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#009E73",
                               "#D55E00"),
                    labels=c("Belden's Landing", "Rio Vista"))

#use cowplot to align stacked plots
aligned <- align_plots(figA, figB,align = "v")
plot_grid(figA, figB, ncol = 1,align = "v")

leveneTest(log(amph2$count+1) ~ amph2$Treatment)
#variances not significantly different


res_aov2 <- aov(log((count+1)) ~ Treatment, data = amph2)

hist(res_aov2$residuals)
qqPlot(res_aov2$residuals, id = FALSE)
shapiro.test(res_aov2$residuals)#normally distributed

######linear model and anova##### 
modamph <- lm(log((count+1)) ~ Treatment + Site,data = amph2) 
summary(modamph)
plot(modamph)
shapiro.test(residuals(modamph))
modamphova <- Anova(modamph, type = 2, test.statistic = "F") 
kable(modamphova)
summary(modamphova)

amph5 <- amph2 %>% filter(Treatment=='Scrubbed')
modamph5 <- lm(log((count+1)) ~ Site,data = amph5) 
summary(modamph5)

amph6 <- amph2 %>% filter(Treatment=='Exchanged')
modamph6 <- lm(log((count+1)) ~ Site,data = amph6) 
summary(modamph6)

#use cowplot to align stacked plots
aligned <- align_plots(figA, figB,align = "v")
plot_grid(figA,figB,ncol = 1,align = "v")

######MACROINVERTEBRATE COMMUNITY ANALYSIS######
amphcmb <- amph 
amphcmb$Species <- factor(amphcmb$Species)
amphcmb$Species <- fct_collapse(amphcmb$Species, Americorophium = c("Americorophium spinicorne", "Americorophium spp" ),
                                Chironomidae = c("Chironomidae larvae", "Chironomidae pupae"),
                                Hydroptilidae = c("Hydroptilidae larvae", "Hydroptilidae pupae"))
amphcmb = filter(amphcmb,Species!="No amphipod or insects")                          

ggplot(amphcmb, aes(x = Location, y = count, fill = Species)) + geom_col()


# Calculate PERMANOVA comparisons for macroinvert communities by treatment and station
# Results show difference in communities by treatment and station

amph3 <- pivot_wider(amphcmb,id_cols = c(Location, Site, Date, Treatment),
                     values_fn = sum,
                     names_from = "Species", 
                     values_from = "total_count",
                     values_fill = 0)

#remove samples with no organisms
amph3 = filter(amph3,Location!="Cage 7")

adon.results <- adonis2(amph3[c(5:20)] ~ amph3$Treatment + amph3$Site,
                        method = "bray",
                        perm = 999)
#write.csv(adon.results, file = "adon.results.csv")

dm_amph3 <- vegdist(amph3[c(5:20)], method = "bray")

bd <- betadisper(dm_amph3, amph3$Treatment)

anova(bd)
permutest(bd)

bd2 <- betadisper(dm_amph3, amph3$Site)

anova(bd2)
permutest(bd2)

######Standardizing by relative abundance and grouping rare taxa#####

# Calculate relative abundance of count by taxon
df_invert_RA <- amphcmb %>%
  group_by(Location, Site, Treatment) %>%
  mutate(MeanRelAbund = total_count/sum(total_count)) %>%
  ungroup

# Highlight most abundant genera
df_invert_RA <- df_invert_RA %>%
  mutate(Taxa = case_when(MeanRelAbund > 0.02 ~ Species,
                          TRUE ~ 'Other'))

length(unique(df_invert_RA$Taxa))

# lump together all "other" taxa
df_invert_RA_tot <- df_invert_RA %>%
  group_by(Location, Date, Site, Treatment, Taxa) %>%
  summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
  ungroup()

ggplot(df_invert_RA_tot, aes(x = Location, y = MeanRelAbund, fill = Taxa)) + geom_col()

# Calculate PERMANOVA comparisons for macroinvert communities by treatment and station using relative abundance (RA)
# with rare taxa (< 2% RA) lumped into "other" catagory
# Results show a difference in communities by Site but not by Treatment

amph4<- pivot_wider(df_invert_RA_tot,id_cols = c(Location, Site, Date, Treatment),
                     values_fn = sum,
                     names_from = "Taxa", 
                     values_from = "MeanRelAbund",
                     values_fill = 0)


adon.results4 <- adonis2(amph4[c(5:16)] ~ amph4$Treatment + amph4$Site,
                        method = "bray",
                        perm = 999)
#write.csv(adon.results, file = "adon.results4.csv")

dm_amph4 <- vegdist(amph3[c(5:16)], method = "bray")

bd4 <- betadisper(dm_amph4, amph4$Treatment)

anova(bd4)
permutest(bd4)

bd6 <- betadisper(dm_amph4, amph4$Site)

anova(bd6)
permutest(bd6)
