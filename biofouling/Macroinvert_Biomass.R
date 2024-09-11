#Convert length to weight for all length measurements (length into a column for coding)
#Length weight regressions per species: Mesomicro spreadsheet (double check names)
#W = aL^b a and b are constants from mesomicro spreadsheet (use a_mg)
#Use formalin (ask if there isn't a formalin option but probably is there for all)

#Then take average weight per species * by total count (exclude mut and frag) = Wet weight

#Wet to carbon -> Carbon weight = Wet weight * 0.25 * 4

#Then same processing as with zoops

#Macro data is breaking out into 4 groups amphipods, tenaid, chyron, isopod (in macro biomass file has crosswalk)
##Americorph.. hyallela... and gam... are Amphipods
#Crosswalk data sheet has amphipods listed out
#Ignore annelids
#Teams meeting has files of interest
#mg=milligrams, convert it to micrograms

library(tidyverse)
library(readxl)
library(data.table)

#setwd("~/Delta Smelt/biofouling coding")

biomass_con <- read_xlsx("data/Mesomicromacro Biomass Conversions Aug2024.xlsx", sheet = "Macro-zooplankton")
crosswalk <- read.csv("data/crosswalk 1.csv")
amph_crosswalk <- read.csv("data/amph_sample_species.csv")

#read in and format field data
#amphdata <- read_xlsx("data/ICF_zoops_data_cages.xlsx", sheet = "Amph Data")
#I hope this is the same data
amphdata <- read_xlsx("data/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx", sheet = "Amph Data")

a <- which(amphdata$pc_frag_mut == "frag")
b <- which(amphdata$pc_frag_mut == "mut")
amph_exc_frag_mut <- amphdata[-c(a,b),]

#combined count of each species by site, does not include lengths taken
amph_counts <- amph_exc_frag_mut %>%
  select(Location, Date, Time, Site, Species, count, starts_with("m")) %>%
  summarize(.by = c(Location, Date, Site, Species), Sum_Total = sum(count))

#Summarized the counts by 
amph_sum_counts <- left_join(amph_counts, select(amph_crosswalk, Species, NewName), by = ("Species")) %>%
  summarize(.by = c(Location, Date, Site, NewName), Count = sum(Sum_Total))

#Converted from wide to long format
amph_lengths <- melt(amph_exc_frag_mut, id=c(1,2,4,8,12), measure = 15:64)

ggplot(amph_lengths, aes(x=Site, y=value, colour=Species)) +
  geom_jitter()

#Convert lengths to wet weights W = aL^b a and b are constants from mesomicro spreadsheet (use a_mg)
amph_crosswalk <- read.csv("data/amph_sample_species.csv")

amphs2 <- left_join(amph_lengths, select(amph_crosswalk, Species, NewName), by = ("Species"))

ggplot(filter(amphs2, NewName !=  "Ignore"), aes(x=Site, y=value , colour=NewName)) +
  geom_jitter()

resies <- matrix(ncol=1, nrow = length(amphs2$Species))
for (i in 1:length(amphs2$Species)) {
  name <- amphs2$Species[i]
  name <- amph_crosswalk[which(amph_crosswalk$Species == name), 2]
  a <- biomass_con[which(biomass_con$Taxname == name),]
  b <- a[which(a$Preservative == "Formalin"),11]
  a <- a[which(a$Preservative == "Formalin"),10]
  resies[i,1] <- as.numeric(0.1*(a*amphs2$value[i]^b))
}
amphs2$Carbon_weight = resies

str(amphs2)
ggplot(filter(amphs2, NewName !=  "Ignore"), aes(x=Site, y=Carbon_weight, colour=NewName)) +
 geom_jitter()

#Investigate
ave_biomass_mean <- amphs2 %>%
  group_by(NewName, Site) %>%
  summarize(Carbon_ave = mean(Carbon_weight, n=n(), na.rm = T))

ave_biomass_median <- amphs2 %>%
  group_by(NewName, Site) %>%
  summarize(Carbon_ave = median(Carbon_weight, na.rm = T))

#use a blanket weight for chironomids of 0.458mg wet weight, 0.0458 carbon
a <- which(ave_biomass_mean$NewName == "Chironimidae")
ave_biomass_mean[a,3] <- 0.0458

a <- which(ave_biomass_median$NewName == "Chironimidae")
ave_biomass_median[a,3] <- 0.0458

#multiply the average biomass by the count of each species per day
#average biomass by mean
mult_mean <- matrix(ncol=1, nrow=length(amph_sum_counts$Count))
for (i in 1:length(amph_sum_counts$Count)) {
  b <- ave_biomass_mean[which(amph_sum_counts$NewName[i] == ave_biomass_mean$NewName),]
  c <- amph_sum_counts$Count[i]
  d <- b[which(as.character(amph_sum_counts[i,3]) == b$Site),3]*c
  mult_mean[i,1] <- d[[1]]
}

#average biomass by median
mult_med <- matrix(ncol=1, nrow=length(amph_sum_counts$Count))
for (i in 1:length(amph_sum_counts$Count)) {
  b <- ave_biomass_median[which(amph_sum_counts$NewName[i] == ave_biomass_median$NewName),]
  c <- amph_sum_counts$Count[i]
  d <- b[which(as.character(amph_sum_counts[i,3]) == b$Site),3]*c
  mult_med[i,1] <- d[[1]]
}

amph_sum_counts$Biomass_mean <- mult_mean[,1]
amph_sum_counts$Biomass_median <- mult_med[,1]

colnames(amph_sum_counts) <- c("Location", "Date", "Site", "NewName", "Count", "Biomass_mean", "Biomass_median")

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Location, y=Biomass_mean, colour=NewName)) +
  geom_jitter(height = 0, width=.1)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Location, y=Biomass_mean, fill=NewName)) +
  geom_col()

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Location, y=Biomass_median, fill=NewName)) +
  geom_col()
  
ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Site, y=Biomass_mean, colour=NewName)) +
  geom_jitter(height = 0, width=.1)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Site, y=Biomass_mean, fill=NewName)) +
  geom_col()

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Site, y=Count, colour=NewName)) +
  geom_jitter(height = 0, width=.1)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Site, y=Count, fill=NewName)) +
  geom_col()


#Stats? add this to the zoop data or run it by itself? - without the subsamples might not be able to combine
#Linear model of total macroinvert bioimass by cage(biomass ~ treatment + location)
#Odd cages = flip, even = scrub

amph_sum_counts$Location <- as.factor(amph_sum_counts$Location)
amph_sum_counts$Site <- as.factor(amph_sum_counts$Site)
str(amph_sum_counts)

treatments = data.frame(Location = c("Cage 1", "Cage 2", "Cage 3", "Cage 4",
                                     "Cage 5", "Cage 6", "Cage 7", "Cage 8"),
                        Treatment = c("Flip", "Scrub", "Flip", "Scrub",
                                      "Flip", "Scrub", "Flip", "Scrub"))

amph_sum_counts <- left_join(treatments, by = c("Location"), amph_sum_counts)

save(amph_sum_counts, file = "data/amph_sum_counts.RData")

library(lme4)
model1 <- glm(formula = Biomass_median ~ Treatment, filter(amph_sum_counts, NewName !=  "Ignore"), family = gaussian)
summary(model1)
plot(model1)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Treatment, y=Biomass_median, colour=NewName)) +
  geom_jitter(height = 0, width=.1)

model2 <- glm(formula = Biomass_median ~ Site + Treatment, amph_sum_counts, family = gaussian)
summary(model2)
plot(model2)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Treatment, y=Biomass_median, colour=NewName)) +
  facet_wrap(. ~ Site) +
  geom_jitter(height = 0, width=.1)

model3 <- glmer(formula = Biomass_median ~ Site*Treatment + (1|Location), amph_sum_counts, family = gaussian)
summary(model3)
isSingular(model3)
plot(model3)

ggplot(filter(amph_sum_counts, NewName !=  "Ignore"), aes(x=Treatment, y=Biomass_median, colour=NewName)) +
  facet_wrap(Location ~ Site) +
  geom_jitter(height = 0, width=.1)


