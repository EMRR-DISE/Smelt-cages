#looking at the delta smelt diet data from the biofouling cage study
#wanted to see if the two treatments of flipping or scrubbing the cages had a difference in the diets of the fish in the cages
#also had 2 different sites

#hypothesis specifically stated that the amount of amphipods would be different but also wondering about the community in general.

library(tidyr)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

#Read in Diet Data----------
#read in the diet data

diet = read_xlsx("data/Delta Smelt Biofouling Diet Data 2023.xlsx") %>% 
  select(-'Comments') %>%  #remove comments column
  mutate(`Prey Taxa LH Stage` = if_else(`Prey Taxa LH Stage` == "Egg", "Amphipod Egg", `Prey Taxa LH Stage`)) %>% 
  rename(FishID = Tag) #rename the id column so it matches with the dissection data


#how many fish were there diets for?
#have this just so I can make sure I get all the fish

dietn = diet %>% 
  pivot_wider(id_cols = Site: FishID, 
              names_from = `Prey Taxa LH Stage`, 
              values_from = Count, values_fill = 0) %>% 
  group_by(`Cage ID`) %>% 
  summarise(fishn = length(`Cage ID`)) #had 40 fish, 5 per cage
  
#also need to read in the dissection info so can calc fullness and condition

lengths= read_xlsx("data/Biofouling Study Fish Dissections 2023.xlsx") %>% 
  select(-Comments)# remove comments column
  
#merge the 2 data sets so have all in one

diet_lw = merge(diet, lengths, by = c('Treatment', 'FishID' )) %>%
  mutate("Empty" = if_else(`Prey Taxa` == "EMPTY", "Y", "N")) %>%  #add column to signify if it was empty or not
  mutate("LH Stage" = replace_na(`LH Stage`, "")) %>%  #replace the NA in life stage column so we can combine the LH and prey
  mutate("Prey Type Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #add the taxa and life stages together to one column. this adds a weird space at the end which took forever to figure out when removing the columns in the next code
  mutate(Treatment = if_else(Treatment == "Flip", "Exchanged", "Scrubbed")) %>% #changing names of the treatments
  mutate(Location = if_else(Location == "Montezuma", "Belden's Landing", Location)) %>%  #changing names of the locations
  select(Location, Treatment, 'Cage ID', FishID, LabForkLength_mm, LabWeight_g, Fullness, Digestion, Empty, 'Total Contents Weight', 'Prey Taxa LH Stage', 'TaxaGroup', 'Amphipod Taxa Group', Count)   #only select the columns we need and reorder

##Diet by Number----------
  
#make graph with the taxa group by site and flip/ scrub

#pivot wider first to add zeros to prey

totprey = diet_lw%>% 
  filter(`TaxaGroup` != "NA")%>% 
  select(-c(`TaxaGroup`, `Amphipod Taxa Group`)) %>% 
  pivot_wider(names_from = `Prey Taxa LH Stage`, 
              values_from = Count, 
              values_fill = 0) %>% 
  select(-EMPTY) #remove the empty as a prey category

write.csv(totprey, file = "Outputs/cagedietnumber_wide.csv")

#bring back the taxa group from the diet file. Weird way to do this but whatever

taxagrp = read.csv("data/Cage Biomass crosswalk Updated.csv") %>% 
  select(DietCageCode, TaxaGroup) %>% #only keep the taxa groupings since doing count first
  filter(DietCageCode != "", !is.na(DietCageCode)) %>% 
  distinct() %>%
  rename("Prey Taxa LH Stage" = DietCageCode)

#now merge so can do the groupings with the zero prey. first need to pivot longer

#then total by fish

totpreygrp = totprey %>% 
  pivot_longer(cols = `Pseudodiaptomus forbesi copepodid`: `Eurytemora affinis copepodid`,  
               names_to = "Prey Taxa LH Stage", 
               values_to = "Count") %>% 
  left_join(., taxagrp) %>% 
  select(Location: Count, `Prey Taxa LH Stage`, TaxaGroup) %>% 
  group_by(Location, Treatment, `Cage ID`, FishID, `TaxaGroup`) %>%
  summarize(tot_prey = sum(Count))
  
  
#Need mean by site and flip/scrub. 
#need to total by cage first

cagemean = totpreygrp %>% 
  mutate(TaxaGroup = if_else(TaxaGroup %in% c("Corophiidae", "Gammaridea", "Other Amphipod"), "Amphipod", TaxaGroup)) %>% #combine all amphipod taxa
  group_by(Location, Treatment, `Cage ID`, TaxaGroup) %>%
  summarize(mean_prey = mean(tot_prey))

#mean by treatment

diet_means = cagemean %>% 
  group_by(Location, Treatment, TaxaGroup) %>%
  summarize(mean_prey = mean(mean_prey))

treat1 = ggplot(diet_means, aes(x = Treatment, y = mean_prey))
treat2 = treat1 + geom_col(position = "fill", aes(fill = TaxaGroup)) + 
  facet_wrap(facets= vars(Location)) 

treat2

ggsave(plot = treat2, filename = "Graphs/ Mean Cage Diet by Number.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)


#full diet treatment glm

#need to pivot the total prey file longer before test this

totprey_long = totprey %>% 
  pivot_longer(cols = `Pseudodiaptomus forbesi copepodid`: `Eurytemora affinis copepodid`,  
                       names_to = "Prey Taxa LH Stage", 
                       values_to = "Count")


#this is testing each prey category as an independent replicate, not sure what you are doing here. 
full_glm = glm(Count~Treatment + Location, data = totprey_long)
summary(full_glm) 
#signif by location, but not treatment

#if you are asking "Did they eat more things?" you need total CPUE

totprey_long2 = totprey %>% 
  pivot_longer(cols = `Pseudodiaptomus forbesi copepodid`: `Eurytemora affinis copepodid`,  
               names_to = "Prey Taxa LH Stage", 
               values_to = "Count") %>%
  group_by(Treatment, Location, FishID) %>%
  summarize(Count = sum(Count, na.rm =T))

full_glm2 = glm(Count~Treatment + Location, data = totprey_long2)
summary(full_glm2)

#plot by amphipods only------ old code. Need to redo if run again

# amph_means = diet_lw %>% 
#   filter(`Amphipod Taxa Group`!= "NA" & `Amphipod Taxa Group` != "Tanaid") %>% 
#   group_by(Treatment, Location, `Amphipod Taxa Group`) %>% 
#   summarise(mean_prey = mean(Count))
# 
# 
# amph1 = ggplot(amph_means, aes(x = Treatment, y = mean_prey))
# amph2 = amph1 + geom_bar(stat = "identity", aes(fill = `Amphipod Taxa Group`)) +
#   facet_wrap(vars(Location))
# 
# amph2

#stats on scrubbing

# amph_glm = glm(Count~Treatment*Location,
#                data = (filter(diet_lw, `Amphipod Taxa Group`!= "NA"))) 
#   
# summary(amph_glm)

#Not significant between treatments, or location

#graph & sep by cages


cage1 = ggplot(cagemean, aes(x = `Cage ID`, y = mean_prey))
cage2 = cage1 + geom_bar(stat = "identity", aes(fill = TaxaGroup)) + 
  facet_wrap(Location~`Treatment`, scales = "free_x") 

cage2

ggsave(plot = cage2, filename = "Graphs/ Diets by Cage1.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)



#Meso Biomass------

#calc biomass using our conversions so that the zoop and diets are more comparable

#read in the biomass conversions file
#this is just for meso so far

bm_conversions = read.csv("data/Cage Biomass crosswalk Updated.csv")  %>%
  select(CageCode, Carbon_weight_ug, TaxaGroup, WetWeight_g) %>%
  filter(!is.na(CageCode)) %>%
  distinct()
  

diet_meso_bm = totprey_long %>% 
  rename("CageCode" = 'Prey Taxa LH Stage') %>% 
  left_join(., bm_conversions, by = "CageCode") %>% 
  filter(`TaxaGroup` != "NA" & `TaxaGroup` != "Other Amphipod" & `CageCode` != "Tanaidacea" & 
           `TaxaGroup` != "Corophiidae" & `TaxaGroup` != "Gammaridea " ) %>% 
  filter(`CageCode` != "Aphididae (winged) adult") %>% 
  mutate(Biomass = Count* Carbon_weight_ug)   #make biomass column 


#Zoop & Amphipods------

# need to figure out the biomass for amphipods in the diets 
#don't have lengths from in the diets but do have them from the ones on the cage and in the zoop data
#read that in to see the average length and use that to calculate biomass

amphzoop = read_xlsx("data/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx", sheet = "Amph Data") %>% 
  mutate(Treatment = if_else(Location %in% c("Cage 1", "Cage 3", "Cage 5", "Cage 7"), "Flip", "Scrub")) %>% #make a column of treatment
  filter(Species %in% c("Americorophium spinicorne", "Gammarus daiberi", "Americorophium spp", "Hyalella spp")) %>%  #filter out to just the amphipods 
  filter(is.na(pc_frag_mut)) %>%  #filter out the amphipods that are just fragments and don't have measurements
  mutate(Species = if_else(Species == "Hyalella spp", "Hyalella", Species )) %>% #removing spp from hyalella to match conversions
  mutate(Species = if_else(Species == "Americorophium spp", "Americorophium spinicorne", Species )) %>% 
  mutate(Treatment = if_else(Treatment == "Flip", "Exchanged", "Scrubbed")) %>% #changing names of the treatments
  mutate(Site = if_else(Site == "Montezuma", "Belden's Landing", Site)) %>%  #changing names of the locations
  rename(Taxname = "Species") %>% 
  select(Site, Treatment, Location, Taxname, m_1:m_50) 

#convert to long to get averages, and then average lengths

amphzoop_long = pivot_longer(amphzoop, cols = c(m_1:m_50), names_to = "orgnum", values_to = "length") %>% 
  filter(length != "NA") %>%
  filter(length <2 | length ==2) #filtering out any large amphipods going off the average that they ate based on the shipchannel diet data 


avg_amph = amphzoop_long %>%
  group_by(Site, Treatment, Taxname) %>%
  summarize(meanlength = mean(length, na.rm =T)) #na.rm removes the nas from the average calc

#Amph Biomass-----------
#convert to biomass using the length weight equations
#updated this file 

macro_convers = read_xlsx("data/Mesomicromacro Biomass conversions Aug2024.xlsx", sheet = "Macro-zooplankton") %>% 
  filter(Preservative == "Ethanol" & Weight_type == "Wet") %>% 
  select(Taxname, a_grams, b)
  

#convert to biomass
#need to do this before averaging so merge the original amph file and macro file

amph_bm = merge(amphzoop_long, macro_convers, by = "Taxname" ) %>% 
  filter(length != "NA") %>% 
  mutate("L^b" = (length^b)) %>% 
  mutate(Weight_g = (a_grams*`L^b`)) %>% 
  mutate(Weight_ug = (Weight_g*1000000)) %>%  #find weight and then convert to ug)
  mutate(Biomass = Weight_ug*.25*.4) %>% 
  rename("WetWeight_ug" = Weight_ug) %>% #maintain the weight column so I can calc fullness later
  select(Site, Treatment, Location, Taxname, length, WetWeight_ug, Biomass)

avg_amphbm = amph_bm %>% 
  group_by(Site, Treatment, Taxname) %>% 
  summarise(mean_bm = mean(Biomass),
            mean_wetwt = mean(WetWeight_ug))

#missing the basic amphipoda category here when combining for biomass. Make a new df so can add to the macrobm that is just the average of all the amphipods from that location/treatment

amphipoda = amph_bm %>% 
  group_by(Site, Treatment) %>% 
  summarise(mean_bm = mean(Biomass), 
            mean_wetwt = mean(WetWeight_ug)) %>% 
  mutate(Taxname = "Amphipod")

avg_amphbm2 = rbind(avg_amphbm, amphipoda)   #combined df with amphipoda broader category
   
  
#need to move these average biomass values to the diet data by taxa

#first need to make an amphipod group column in the total prey file to limit it to the macro prey

totprey_long2 =  totprey_long %>%  
  mutate("AmphipodTaxa" = (case_when(`Prey Taxa LH Stage` == "Gammaridea" ~ "Gammarus daiberi", 
                                       `Prey Taxa LH Stage` == "Hyalella azteca" ~ "Hyalella",
                                       `Prey Taxa LH Stage` == "Americorophium spinicorne" ~ "Americorophium spinicorne",
                                       `Prey Taxa LH Stage` == "Corophiidae juvenile" ~ "Americorophium spinicorne", #only   americorophium species is spinicorne so these are likely juveniles of this species
                                       `Prey Taxa LH Stage` == "Amphipoda" ~ "Amphipod", 
                                       `Prey Taxa LH Stage` == "Tanaidacea"  ~ "Tanaid"))) %>% 
  mutate("AmphipodTaxa" = if_else(`FishID` %in% c('C3N18', 'C6N12', 'C6N41', 'C8N25', 'C8N26')  & `Prey Taxa LH Stage` ==                                            "Gammaridea" & Count >0, "Eogammarus", `AmphipodTaxa`)) #make sure to keep the eogammarus taxa in here

#join the average amph data from the zoop to the diets

diet_macbm = totprey_long2 %>% 
  filter(`AmphipodTaxa` != "NA") %>% 
  rename("Site" = Location) %>% 
  rename(Taxname = AmphipodTaxa) %>% 
  left_join(., avg_amphbm2, by = c("Site", "Treatment", "Taxname")) %>% 
  select(-`Prey Taxa LH Stage`) %>% 
  mutate(mean_bm = (case_when(Count == 0 ~ 0,
                              Taxname == "Gammarus daiberi" | Taxname == "Eogammarus"~ 5.7926225, #change the other gammarus taxa to g daiberi since thats probably what it is and thats what we have lengths for. also don't have eogammarus so use g.daiberi
                              Site == "Rio Vista" & Taxname == "Hyalella"~ 15.920186, #add in the biomasses that have NAs because don't have exact matchs
                              Site == "Rio Vista" & Taxname == "Amphipod"~ 13.19296767, 
                              Taxname == "Americorophium spinicorne" ~ 10.93771267, 
                              Taxname == "Tanaid" ~ 10.93771267))) %>%   #the carbon weights for tanaid which were converted to wet weights from diet study were 4x what an amphipod would be, assumed Tanaid is the same size as the 2mm amphipods from the same area which happens to be g. daiberi
  rename("Carbon_weight_ug" = mean_bm) %>% #rename column with biomass from the amph data to carbon weight so can multiple by the count for tot mac biomass
  mutate(Biomass = Count* Carbon_weight_ug) %>% 
  mutate("TaxaGroup" = case_when(Taxname == "Gammarus daiberi" | Taxname == "Eogammarus" | Taxname == "Hyalella"~ "Gammaridea", 
                                 Taxname == "Americorophium spinicorne" ~ "Corophiidae", 
                                 Taxname == "Gammarus daiberi" | Taxname == "Eogammarus"~ "Gammaridea", 
                                 Taxname == "Tanaid" ~ "Tanaid", 
                                 Taxname == "Amphipod" ~ "Other Amphipod")) %>%  #make a taxa group column for the combined graphs

  mutate("WetWeight_g" = (Biomass/(.25*.4)/1000000)) %>%  #make a wet weight column so we can calc fullness later
  select(-c(mean_wetwt))


#Total Diet Biomass----------

#add macrobm and meso biomass together

diet_totbm = diet_macbm  %>% 
  rename("Location" = "Site") %>% 
  full_join(., diet_meso_bm) %>% 
  select(-c(CageCode, Taxname))


#averages. need to do in steps. total by fish, then average by cage, then treatment and location

fishbm = diet_totbm %>% 
  mutate(TaxaGroup = if_else(TaxaGroup %in% c("Corophiidae", "Gammaridea", "Other Amphipod"), "Amphipod", TaxaGroup)) %>% #combine all amphipod taxa
  group_by(Location, Treatment, `Cage ID`, FishID) %>% 
  summarize(tot_fishbm = sum(Biomass))

write.csv(diet_totbm, "data/cagedietbiomass.csv")

#boxplots 

#total by cage--- use this for the boxplot graph

cagebm = fishbm %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(avg_cagebm = mean(tot_fishbm))

#total for treatment

treatbm = cagebm %>% 
  group_by(Location, Treatment) %>% 
  summarise(avg_treatbm = mean(avg_cagebm))

#boxplots 

tbm1 = ggplot(cagebm, aes(x = Treatment, y = `avg_cagebm`, fill = Location))
tbm2 = tbm1 + geom_boxplot() + 
  ylab("Average Diet Biomass (µgC)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))

tbm2

#######WOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO This graph looks great.

ggsave(plot = tbm2, filename = "Graphs/Cage Diet Biomass Boxplot.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)

#####Caveats for this graph############

#To get here, assumed amphipods were <2mm in the stomach since thats what the average was for fish in Aug to Oct in actual diet data. Also assumed that tanaids were the same as the amphipods biomass
#removed some random critters, look at the crosswalk for what was removed and why
#chironomids used average wet from cdfw diet data instead of LW equation because there are so many conflicting ones 


#stacked plots

taxbm = diet_totbm %>% 
  mutate(TaxaGroup = if_else(TaxaGroup %in% c("Corophiidae", "Gammaridea", "Other Amphipod"), "Amphipod", TaxaGroup)) %>% #combine all amphipod taxa
  group_by(Location, Treatment, `Cage ID`, FishID, `TaxaGroup`) %>% 
  summarize(tot_taxbm = sum(Biomass))

txcagebm = taxbm %>% 
  group_by(Location, Treatment, `Cage ID`, `TaxaGroup`) %>% 
  summarise(avg_txcagebm = mean(tot_taxbm))

txtreatbm = txcagebm %>% 
  group_by(Location, Treatment, `TaxaGroup`) %>% 
  summarise(avg_txtreatbm = mean(avg_txcagebm))


treatbm1 = ggplot(txtreatbm, aes(x = Treatment, y = avg_txtreatbm))
treatbm2 = treatbm1 + geom_bar(stat = "identity", aes(fill = `TaxaGroup`)) + 
  facet_wrap(facets= vars(Location)) +
  ylab("Diet Biomass (µgC)")

treatbm2

ggsave(plot = treatbm2, filename = "Graphs/Cage Diet Stacked Biomass.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)


#relative biomass
treatrbm1 = ggplot(txtreatbm, aes(x = Treatment, y = avg_txtreatbm))
treatrbm2 = treatrbm1 + geom_col(position = "fill", aes(fill = `TaxaGroup`)) + #geom_col and position fill means that it gives the relative biomass
  facet_wrap(facets= vars(Location)) +
  ylab("Relative Diet Biomass (µgC)")

treatrbm2

ggsave(plot = treatrbm2, filename = "Graphs/Cage Diet Relative Biomass.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)


#Amphipod Differences------


#want to see if there were differences in the amphipods in the diets, both in numbers and in biomass

#total by fish and then test that by treatment and location
amphnumb_fish = totprey_long2 %>% 
  filter(AmphipodTaxa !="NA") %>% 
  select(-`Prey Taxa LH Stage`) %>% 
  group_by(Location, Treatment, `Cage ID`, FishID) %>% 
  summarise(tot_amphfish = sum(Count))


#glm on amphipods

amphglm = glm(tot_amphfish~ Treatment + Location, data = amphnumb_fish)
summary(amphglm)
plot(amphglm)

ggplot(amphnumb_fish, aes(x = Treatment, y = tot_amphfish, fill = Location))+
  geom_boxplot() + 
  ylab("Average Amphipods (#/fish)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))


#boxplots 

amphnumb_cage = amphnumb_fish %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(tot_amphcage = sum(tot_amphfish), 
            avg_amphcage = mean(tot_amphfish))

amphnumb1 = ggplot(amphnumb_cage, aes(x = Treatment, y = avg_amphcage, fill = Location))
amphnumb2 = amphnumb1+ geom_boxplot() + 
  ylab("Average Amphipods (#/fish)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))

amphnumb2 

#stacked plots


amphnumb_trt = amphnumb_cage %>% 
  group_by(Location, Treatment) %>% 
  summarise(avg_amphtrt = mean(tot_amphcage))
  

  
  


#Fullness--------------

#look at gut weight to see if we had fuller stomachs between treatments/ sites

##Total GC Weight (Actual fullness)------

#look at total gut content fullness based on the weights of the contents
#this is somewhat problematic since some of the weights were zero despite having food in their stomacsh

##Actual Fullness--------

actfullness = diet_totbm %>% 
  filter(`Total Contents Weight` >0 &
           Empty == "N") %>% #removing the ones that couldn't be weighed but weren't empty
  group_by(Location, Treatment, `Cage ID`, FishID, LabWeight_g, `Total Contents Weight`) %>% 
  summarize(tot_fishbm = sum(Biomass), 
            tot_fishwetwt = sum(WetWeight_g, na.rm =T)) %>% 
  mutate("ActFullness (%BW)" = ((`Total Contents Weight`/LabWeight_g)*100)) %>%  #make a column of % GF as a function of body weight. 
  mutate("Gut Fullness (%)" = ((`ActFullness (%BW)`)/2 )*100) #2% is considered full so divide by 2 to get better numbers to interpret
#some issues with this given that the total contents weight is sometimes zero or close to it

#avg by cage--- use this for the boxplot graph

cageafull = actfullness %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(avg_cageaf = mean(`Gut Fullness (%)`))


#boxplots 

taf1 = ggplot(cageafull, aes(x = Treatment, y = `avg_cageaf`, fill = Location))
taf2 = taf1 + geom_boxplot() + 
  ylab("Average Actual Gut Fullness (%)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))

taf2

#glm for fullness on treatments. No difference between treatments or locations 

treatafull_glm = glm(`Gut Fullness (%)` ~Treatment + Location, data = actfullness)

summary(treatafull_glm)

#average fullness by cage. There was one cage that had heaps of mortality---- cage 5 but seems fine with fullness
# 
# avg_cagefullness = fullness %>% 
#   mutate(`Cage ID` = as.character(`Cage ID`)) %>% 
#   group_by(Location, Treatment, `Cage ID`) %>% 
#   summarise(avg_fullness = mean(`Percent Fullness (%BW)`))
# 
# #looking at treatment and location only
# 
# avg_treatfull = avg_cagefullness %>% 
#   group_by(Location, Treatment) %>% 
#   summarise(avg_fullness = mean(avg_fullness))



# #graph by cage
# 
# cage_full1 = ggplot(fullness, aes(x = `Cage ID`, y = `Percent Fullness (%BW)`))
# cage_full2 = cage_full1 + geom_boxplot() + 
#   facet_wrap(~Treatment)
# 
# cage_full2

#fullness graph by treatment

# tfull1 = ggplot(fullness, aes(x = `Treatment`, y = `Gut Fullness (%)`, , fill = Location))
# tfull2 = tfull1 + geom_boxplot()+
#   scale_fill_manual(values = c("#1B9E77","#D95F02")) #these are the colors we're using for the paper
# 
# tfull2

##Calculated Fullness--------------------

#look at calculated fullness based off of biomass
#use calc biomass file and the equation diet weight/fish body weight *100


#take the total biomass file and convert to wet weights, then calc fullness
#calc by fish first

calcfullness = diet_totbm %>% 
  group_by(Location, Treatment, `Cage ID`, FishID, LabWeight_g) %>% 
  summarize(tot_fishbm = sum(Biomass), 
            tot_fishwetwt = sum(WetWeight_g)) %>% 
  mutate("Percent Fullness (%BW)" = ((tot_fishwetwt/LabWeight_g)*100)) %>%  #make a column of % GF as a function of body weight. 
  mutate("Gut Fullness (%)" = ((`Percent Fullness (%BW)`) )*100) #2% is considered full so divide by 2 to get better numbers to interpret

#calc fullnessboxplots 

#avg by cage--- use this for the boxplot graph

cagecfull = calcfullness %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(avg_cagecf = mean(`Gut Fullness (%)`))


#boxplots 

tcf1 = ggplot(cagecfull, aes(x = Treatment, y = `avg_cagecf`, fill = Location))
tcf2 = tcf1 + geom_boxplot() + 
  ylab("Average Calc Fullness (%)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))

tcf2

ggsave(plot = tcf2, filename = "Graphs/ Calc Fullness Boxplot.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)

save(cagecfull, file = "data/GutFullness_cagecfull.RData")

#glm of the calculated fullness
#treatments are signif diff, not loc

calcglm = glm(`Gut Fullness (%)`~Treatment + Location, data = calcfullness)
summary(calcglm)

##Qualitative Fullness------

#graph of the fullness categories to see if they make sense

#use the original diet dataframe

ggplot(diet, aes(x = Treatment, y = Fullness, fill = Site))+
  geom_boxplot()

#looks horrible and not very useful---- will leave off


###################################Exploring Wet Weights##########################################

#the amphipod lengths and biomasses are super inflating the diet biomass which seems to mask the limno/pseudo differences
#especially for the BDL scrubbed cages. 

#explore if calc wet weights as would with diet databases to see if that helps and can compare to the measured gut content weight

#Wet Weights--------------

#pulled the wet weight zoop data from the diet databases and added to crosswalk

wet_convers = read.csv("data/Cage Biomass crosswalk Updated.csv")  %>%
  select(DietCageCode, Carbon_weight_ug, TaxaGroup,  WetWeight_g) %>%
  filter(!is.na(DietCageCode)) %>%
  distinct()


diet_mesowet = diet_lw %>% 
  rename("DietCageCode" = 'Prey Taxa LH Stage') %>% 
  left_join(., wet_convers, by = c("DietCageCode", "TaxaGroup")) %>% 
  #filter(`Taxa Group` != "NA" & `Taxa Group` != "Amphipod") %>% 
  #filter(`CageCode` != "Aphididae (winged) adult" & `CageCode` !="EMPTY") %>% 
  mutate(WetWeightbyCount_g = Count* WetWeight_g)  #make column for wet weight by the prey count 
  
#add up prey wet weights by fish for total gut content weight by fish

totmes_wet = diet_mesowet %>% 
  group_by(Location, Treatment, `Cage ID`, FishID, LabForkLength_mm,  
           LabWeight_g, Fullness, Digestion,`Total Contents Weight`) %>%   
  summarize(totalwetwt = sum(WetWeightbyCount_g, na.rm =T))


avg_cagewet = totmes_wet %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(avg_cagewet_mg = (mean(totalwetwt))*1000)

avg_treatwet = avg_cagewet %>% 
  group_by(Location, Treatment) %>% 
  summarise(avg_treatwet_mg = mean(avg_cagewet_mg))

#boxplot of wet weight

wetwt1 = ggplot(avg_cagewet, aes(x = Treatment, y = `avg_cagewet_mg`, fill = Location))
wetwt2 = wetwt1 + geom_boxplot() + 
  ylab("Average Diet Wet Weight (mg)")

wetwt2



#stacked plots with wet weight

#now wet weight by taxa
totmes_wet2 = diet_mesowet %>% 
  group_by(Location, Treatment, `Cage ID`, FishID, LabForkLength_mm, TaxaGroup, 
           LabWeight_g, Fullness, Digestion,`Total Contents Weight`) %>%   
  summarize(totalwetwt = sum(WetWeightbyCount_g, na.rm =T))


avg_cagewet2 = totmes_wet2 %>% 
  group_by(Location, Treatment, `Cage ID`, TaxaGroup) %>% 
  summarise(avg_cagewet_mg = (mean(totalwetwt))*1000)

avg_treatwet2 = avg_cagewet2 %>% 
  group_by(Location, Treatment, TaxaGroup) %>% 
  summarise(avg_treatwet_mg = mean(avg_cagewet_mg))



treatwet1a = ggplot(avg_treatwet2, aes(x = Treatment, y = avg_treatwet_mg))
treatwet2a = treatwet1a + geom_bar(stat = "identity", aes(fill = `TaxaGroup`)) + 
  facet_wrap(facets= vars(Location)) 

treatwet2a










