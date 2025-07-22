#checking out the 2024 cage diet results

#plus want to compare all summer/fall cage studies (2019, 2023, 2024)

#copying mostly from the "biofouling diet" code from 2023 analyses

library(tidyr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)


#Read in Diet Data----------
#read in the diet data
#all in the same file, but with each tab as a new year. read them in individually since need to clean them up

#for each file: 1.combine the prey type with the lifestage and remove the na, 2. add month and year columns, 3. convert dates, 4. add a column to all for experiment/ study

##2024----

diet2024 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Delta Smelt Cage Diets 2019-2024_CEB.xlsx", sheet = "2024") %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #add the taxa and life stages together to one column. 
  mutate(Date = as.Date(Date), 
         Month = as.factor(month(Date)), 
         Year = as.factor(year(Date)), 
         'CageID' = as.character(`Cage ID`), 
         Study = "SMSCG", 
         StudyCode = "SMS") %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) #create a fish ID so can be conected to other files


# check the fish n to make sure its all included later
n2024 = diet2024 %>% 
  select(`FishID`) %>% 
  unique() #54 fish

#2023 alreaDY has some edits and cleaning up from last year so decided to copy in the updated file I worked with last year which has a lot of the already needed things. Need to remove the taxa and amphipod groups though

##2023-----

diet2023 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Delta Smelt Cage Diets 2019-2024_CEB.xlsx", sheet = "2023 Updated") %>%
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>% 
  mutate(Date = as.Date(Date), 
         Month = as.factor(month(Date)), 
         Year = as.factor(Year), 
         CageID = as.character(`Cage ID`), 
         Study = "Biofouling Study", 
         StudyCode = "BFS") %>%
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>%  #create a fish ID so can be conected to other files
  select(- c(TaxaGroup, 'Amphipod Taxa Group'))

n2023 = diet2023 %>% 
  select(`FishID`) %>% 
  unique() #40 fish

##2022-----

diet2022 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Delta Smelt Cage Diets 2019-2024_CEB.xlsx", sheet = "2022") %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #add the taxa and life stages together to one column. 
  mutate(Date = as.Date(Date), 
         Month = as.factor(month(Date)), 
         Year = as.factor(year(Date)),
         Site = "Rio Vista", #add a column adding that it was done in RV
         Study = "Feeding Study",  #add a column that specifies its the feeding experiment
         StudyCode = "FDS") %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>%  #create a fish ID so can be conected to other files
  rename(CageID = `Cage ID`, 
         Treatment = `Feed Experiment`)  #change the type of feed column to treatment to match the biofouling

n2022 = diet2022 %>% 
  select(`FishID`) %>% 
  unique() #250 fish

#found some duplicates in 2022 and 2019. Super weird since it looks like they had multiple lines with the same fish and taxa, but different counts

dups22 = diet2022%>% 
  group_by(`FishID`, `Prey Taxa LH Stage`) %>% 
  summarize(n = n()) %>% 
  filter(n>1)

#6 duplicates. 

##2021----

#need to add treatment to this data too
  
diet2021 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Delta Smelt Cage Diets 2019-2024_CEB.xlsx", sheet = "2021") %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #add the taxa and life stages together to one column. 
  rename(Site = Location, 
         Study = Experiment, 
         "TaxaWeight" = `Taxa weight`, 
         CageID = `Cage ID`) %>% 
  mutate(Date = as.Date(Date), 
         Month = as.factor(month(Date)), 
         Year = as.factor(year(Date)), 
         Study = "Domestication/Behavior", 
         StudyCode = "DBS") %>% 
  mutate(Treatment = case_when(str_detect(`CageID`, "C") ~ "High",
                               str_detect(`CageID`, "D") ~ "Low", 
                               str_detect(`CageID`, "E") ~ "High")) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_"))

n2021 = diet2021 %>% 
  select(`FishID`) %>% 
  unique() #22 fish

##2019----

#need to add in the study names based on the date, plus the different treatments

#had two fish with legit duplicates RL96 had duplicated taxon and counts, so updated it in the excel sheet.
#RH18 had Acantho adult twice for counts 4 and 49, and then Acantho copepodid twice with the same counts. Removed it so that there are 53 total Acantho but undifferentiated life stage. Rest of the prey categories for that fish were duplicated exactly

diet2019 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Delta Smelt Cage Diets 2019-2024_CEB.xlsx", sheet = "2019 Updated") %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  
  rename(Site = Location, 
         Study = Experiment, 
         CageID = `Cage ID`, 
         TaxaWeight = 'Taxa Weight') %>% 
  mutate(Date = as.Date(Date), 
         Month = as.factor(month(Date)), 
         Year = as.factor(year(Date)), 
         Study = case_when(Date %in% c("2019-11-06","2019-11-07") ~ "Suisun Marsh Pilot",
                  Date %in% c("2019-08-28") ~ "North Delta Pilot",
                  .default = "Enclosure Prototype"), 
         StudyCode = case_match(Study, "Suisun Marsh Pilot" ~ "SMP", 
                                "North Delta Pilot" ~ "NDP", 
                                "Enclosure Prototype" ~ "ENP" )) %>% 
  mutate(StudyCode = case_when(StudyCode == "ENP" & Site == "Sac DWSC" ~ "ENPSDWSC", 
                               StudyCode == "ENP" & Site == "RIVERS" ~ "ENPRV", 
                               .default = StudyCode)) %>% 
  mutate(Treatment = case_match(`CageID`, "A" ~ "Wrap", 
                                "B"~ "Large", 
                                "C" ~ "Small", 
                                "D" ~ "Small", 
                                "E" ~ "Wrap", 
                                "F" ~ "Large"),
         Treatment = case_when(str_detect(`CageID`, "FCCL") ~ "Control", 
                               .default = Treatment)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_"))

n2019 = diet2019 %>% 
  select(`FishID`) %>% 
  unique() #250 fish

#Stomach Weights--------------

#Arielle sent an updated spreadsheet with all the stomach weights on June 12 2025
#she took stomach weights in all years but didn't include them since it wasn't part of the contract, but part of their protocol
#want to add these so we can publish with them
#Description of the categories:
# Full weight is what it weighs right out of the fish, and before you take out the contents
# Empty stomach weight is after you process the guts
# Difference is just those weights subtracted
# Total contents weight is when you weighed the prey that you dissected from the stomach, which is sometimes 0 due to the size of the prey and the scale not precise enough to pick it up…. Or its plant material or something. Sometimes the difference in the two weights despite no contents is water 

#need to read in each sheet and then combine, and then merge with the larger dataframe for one large dataset
#need to make a fish ID so can connect it
#also need to make sure the column types match since some weights have NAs. All should be a character
#need to make sure to join with the taxa column too

stomachs2024 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/DSM Cage Diets 2019-2024 Stomach Wts_New.xlsx", sheet = "2024") %>% 
  mutate(Date = as.Date(Date), 
         Year = as.factor(year(Date)),
         StudyCode = "SMS", 
         CageID = as.character(`Cage ID`)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #need to have this so I can join the diets without it freaking out
  rename("FullStomachWeight" = `Stomach weight (g) full`, 
         "EmptyStomachWeight" = `Stomach weight (g) empty`, 
         "StomachWeightDifference" = `Difference of full and empty`) %>% 
  mutate("StomachWeightDifference" = as.character(StomachWeightDifference), 
         "FullStomachWeight" = as.character(FullStomachWeight), 
         "EmptyStomachWeight" = as.character(EmptyStomachWeight)) %>% 
  select(Year, Date, Tag, CageID, FishID, `FullStomachWeight`, `EmptyStomachWeight`, `StomachWeightDifference`,'Total Contents Weight', 'Prey Taxa LH Stage')

stomachs2023 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/DSM Cage Diets 2019-2024 Stomach Wts_New.xlsx", sheet = "2023") %>% 
  mutate(Date = as.Date(Date), 
         Year = as.factor(year(Date)),
         StudyCode = "BFS", 
         CageID = as.character(`Cage ID`)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #need to have this so I can join the diets without it freaking out
  rename("FullStomachWeight" = `Stomach weight (g) full`, 
         "EmptyStomachWeight" = `Stomach weight (g) empty`, 
         "StomachWeightDifference" = `Difference of full and empty`)%>% 
  mutate("StomachWeightDifference" = as.character(StomachWeightDifference), 
         "FullStomachWeight" = as.character(FullStomachWeight), 
         "EmptyStomachWeight" = as.character(EmptyStomachWeight)) %>% 
  select(Year, Date, Tag, CageID, FishID, `FullStomachWeight`, `EmptyStomachWeight`, `StomachWeightDifference`,'Total Contents Weight', 'Prey Taxa LH Stage')



stomachs2022 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/DSM Cage Diets 2019-2024 Stomach Wts_New.xlsx", sheet = "2022") %>% 
  mutate(Date = as.Date(Date), 
         Year = as.factor(year(Date)),
         StudyCode = "FDS", 
         CageID = as.character(`Cage ID`)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #need to have this so I can join the diets without it freaking out
  rename("FullStomachWeight" = `Stomach weight (g) full`, 
         "EmptyStomachWeight" = `Stomach weight (g) empty`, 
         "StomachWeightDifference" = `Difference of full and empty`) %>% 
  mutate("StomachWeightDifference" = as.character(StomachWeightDifference), 
         "FullStomachWeight" = as.character(FullStomachWeight), 
         "EmptyStomachWeight" = as.character(EmptyStomachWeight)) %>% 
  select(Year, Date, Tag, CageID, FishID, `FullStomachWeight`, `EmptyStomachWeight`, `StomachWeightDifference`,'Total Contents Weight', 'Prey Taxa LH Stage', Count)


stomachs2021 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/DSM Cage Diets 2019-2024 Stomach Wts_New.xlsx", sheet = "2021") %>% 
  mutate(Date = as.Date(Date), 
         Year = as.factor(year(Date)),
         StudyCode = "DBS", 
         CageID = as.character(`Cage ID`)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #need to have this so I can join the diets without it freaking out
  rename("FullStomachWeight" = `Stomach weight (g) full`, 
         "EmptyStomachWeight" = `Stomach weight (g) empty`, 
         "StomachWeightDifference" = `Difference of full and empty`)%>% 
  mutate("StomachWeightDifference" = as.character(StomachWeightDifference), 
         "FullStomachWeight" = as.character(FullStomachWeight), 
         "EmptyStomachWeight" = as.character(EmptyStomachWeight)) %>% 
  select(Year, Date, Tag, CageID, FishID, `FullStomachWeight`, `EmptyStomachWeight`, `StomachWeightDifference`,'Total Contents Weight', 'Prey Taxa LH Stage')


stomachs2019 = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/DSM Cage Diets 2019-2024 Stomach Wts_New.xlsx", sheet = "2019") %>% 
  rename(Site = Location, 
         Study = Experiment) %>% 
  mutate(Date = as.Date(Date), 
         Year = as.factor(year(Date)),
         CageID = as.character(`Cage ID`), 
         Study = case_when(Date %in% c("2019-11-06","2019-11-07") ~ "Suisun Marsh Pilot",
                           Date %in% c("2019-08-28") ~ "North Delta Pilot",
                           .default = "Enclosure Prototype"), 
         StudyCode = case_match(Study, "Suisun Marsh Pilot" ~ "SMP", 
                                "North Delta Pilot" ~ "NDP", 
                                "Enclosure Prototype" ~ "ENP" )) %>% 
  mutate(StudyCode = case_when(StudyCode == "ENP" & Site == "Sac DWSC" ~ "ENPSDWSC", 
                               StudyCode == "ENP" & Site == "RIVERS" ~ "ENPRV", 
                               .default = StudyCode)) %>% 
  mutate(`FishID` = paste(Year, StudyCode, Tag, sep = "_")) %>% 
  mutate("Prey Taxa LH Stage" = paste(`Prey Taxa`, `LH Stage`, sep = " ")) %>%  #need to have this so I can join the diets without it freaking out
  rename("FullStomachWeight" = `Stomach weight (g) full`, 
         "EmptyStomachWeight" = `Stomach weight (g) empty`, 
         "StomachWeightDifference" = `Difference of full and empty`) %>% 
  mutate("StomachWeightDifference" = as.character(StomachWeightDifference), 
         "FullStomachWeight" = as.character(FullStomachWeight), 
         "EmptyStomachWeight" = as.character(EmptyStomachWeight)) %>% 
  select(Year, Date, Tag, CageID, FishID, `FullStomachWeight`, `EmptyStomachWeight`, `StomachWeightDifference`,'Total Contents Weight', 'Prey Taxa LH Stage', Count)


###Diets with Weights by Year-----------
#add to the diet data
#doing this separately by year since I made a bunch of edits to the larger dataset after combing that I don't want to have to repeat with the stomach weights

diet2024wt = diet2024 %>% 
  left_join(., stomachs2024, by= c('Year', 'Date', 'Tag', 'CageID', 'FishID', 'Total Contents Weight', 'Prey Taxa LH Stage'))

diet2023wt = diet2023 %>% 
  left_join(., stomachs2023, by= c('Year', 'Date', 'Tag', 'CageID', 'FishID', 'Total Contents Weight', 'Prey Taxa LH Stage'))

diet2022wt = diet2022 %>% 
  left_join(., stomachs2022, by= c('Year', 'Date', 'Tag', 'CageID', 'FishID', 'Total Contents Weight', 'Prey Taxa LH Stage', 'Count'))

diet2021wt = diet2021 %>% 
  left_join(., stomachs2021, by= c('Year', 'Date', 'Tag', 'CageID', 'FishID', 'Total Contents Weight', 'Prey Taxa LH Stage'))

diet2019wt = diet2019 %>% 
  left_join(., stomachs2019, by= c('Year', 'Date', 'Tag', 'CageID', 'FishID', 'Total Contents Weight', 'Prey Taxa LH Stage', 'Count')) #need to add count since there's some funky data with duplicate taxa, but diff counts. Fix later
  
#Combined Diet dataset-----

#combine all the diet data for the years, including stomach weights

#also want to clean up the prey taxa lifestage column by removing the "undetermined" and the NAs from the life stages. Add a space before each of these in the code so that it doesn't leave a weird space at the end
#if I were to mine the comments, this would be the code   
# mutate(Error = case_when(str_detect(`Comments`, "Missing|missing|broken|Broken|ripped") ~ "TRUE",
#                          .default = "FALSE"))

#need to clean up some of the taxa names as well with case when

#add an empty Y or N column 

# need to add an error column for when the stomachs were torn or something but stuff is turning out weird when I code it to search the comments 
#instead I just seatched the cpomments visually to pick out the bad stomach samples and code based on that list

#the counts for all the presence absence categories like plant material and such should be blank

dietallwt = diet2024wt %>% 
  bind_rows(diet2019wt, diet2021wt, diet2022wt,diet2023wt) %>% 
  rename(Taxon = `Prey Taxa LH Stage`,
         TotalContentsWeight = `Total Contents Weight`) %>% 
  mutate(Taxon = str_remove(Taxon, " NA"), 
         Taxon = str_remove(Taxon, " undetermined"), 
         Taxon = str_remove(Taxon, " sp\\.")) %>% # remove the sp. part of the name just to be consistent and need to add the // to show that the period is actually part of the expression
  mutate(Taxon = str_replace(Taxon, "^\\w{1}", toupper)) %>%  #this replaces the first letter in the taxa name with a capital letter. The weird ^\\ expression says to choose the first letter and I guess toupper capitalizes it
  mutate(Taxon = (case_when(Taxon == "Americorophium salmonis" ~ "Americorophium", #need to fix tons of spelling mistakes and taxa consistencies
                            Taxon == "EMPTY" ~ "Empty", 
                            Taxon %in% c("Plant matter", "Plant/Algae matter", "Plant seed") ~ "Plant material",
                            Taxon == "Hyallela azteca" ~ "Hyalella azteca", 
                            Taxon == "Naupli" ~ "Copepod nauplii",
                            Taxon %in% c("Digested material", "Stomach lining/digestive matter") ~ "Digestive material", 
                            Taxon == "Dirt/Plant matter" ~ "Detritis",
                            Taxon %in% c("Egg",  "Eggs",  "Egg unidentified egg") ~ "Unidentified eggs",
                            Taxon == "Harpaticoida" ~ "Harpacticoida",
                            Taxon == "Diaphonosoma" ~ "Diaphanosoma",
                            Taxon == "Acanthocyclops vernalis" ~ "Acanthocyclops",
                            Taxon == "Gammaridea" ~ "Gammaridae",
                            Taxon == "Crustacea" ~ "Animal parts",
                            .default = Taxon)), 
  Taxon = str_replace(Taxon, "affinis", "carolleeae")) %>% #change eury species
  mutate(Site = case_match(Site, "Montezuma" ~ "Suisun Marsh", #make sure all the locations match between years
                            "RV" ~ "Rio Vista",
                            "RIVERS" ~ "Rio Vista",
                            "High RVERS"~ "Rio Vista",
                            "Low RVERS" ~ "Rio Vista",
                            "Sac DWSC" ~ "SDWSC",
                            .default = Site),
          Site = case_when(is.na(Site) & substr(Tag, 1, 2) == "RV" ~ "Rio Vista",
                           is.na(Site) & substr(Tag, 1, 2) == "SM" ~ "Suisun Marsh",
                           is.na(Site) & substr(Tag, 1, 2) == "YB" ~ "Yolo Bypass",
                           TRUE ~ Site)) %>% 
  mutate(Empty = if_else(Taxon == "Empty", "Y", "N"), 
         Taxon = if_else (Taxon == "Empty", "None", Taxon), 
         Count = if_else(Taxon == "None", NA, Count)) %>% #add an empty column. Also need to remove the taxon "empty" and replace the counts with NA
  mutate(Error = if_else(FishID %in% c("2019_NDP_RV_082819_10", "2019_NDP_RV_082819_18",
                                       "2024_SMS_C2N10", "2024_SMS_C2N2", "2024_SMS_C3N19", "2024_SMS_C3N24", 
                                       "2024_SMS_C5N11", "2024_SMS_C5N15", "2024_SMS_C5N4", "2024_SMS_C6N20",
                                       "2024_SMS_C8N14", "2024_SMS_C8N18", "2024_SMS_C8N20"), "TRUE", "FALSE")) %>% #add an error column for samples that had problems with the stomach. Searched the comments manually for these
  mutate(`TotalContentsWeight` = case_when(`TotalContentsWeight` == 0 & `Taxon` != "None" ~ NA, 
                                             .default = `TotalContentsWeight`)) %>%   #changing any stomach weight that wasn't able to be weighed as NA instead of 0  
  mutate(Count= if_else(Taxon %in% c("Animal parts", "Amphipoda parts", "Plant material", "Digestive material", 
                                     "Detritis"), NA, Count)) %>%  #some of the presence/absence categories have 0 or 1 counts, replace with NA
  select(Study, Site, Year, Month, Date, CageID, Treatment, FishID, Tag, Error, Empty, Fullness, Digestion, FullStomachWeight:StomachWeightDifference, TotalContentsWeight, Taxon, Count, `TaxaWeight`, Comments)


#Taxa Names-----

#want a list of taxa names for these years

taxanames = dietallwt %>% 
  select(`Taxon`) %>% 
  unique()

#make a csv of just the taxa names

write.csv(taxanames, "Outputs/cagedietuniquetaxa.csv", row.names = FALSE)


#Duplicates----

#when looking at 2022, we have duplicates so want to check for all of them
#need to deal with these first before adding on the stomach weights

dupsall = dietallwt%>% 
  group_by(`FishID`, Taxon) %>% 
  summarize(n = n()) %>% 
  filter(n>1)

#have multiple combos of fish id and taxon, and when looking at the datasheet it looks like they truly did enter the taxon twice with different counts, which is funky
#fix this by when I pivot wider by doing values_fn = sum
#or I just group all of them and then summarize the counts

dietnewwt = dietallwt %>% 
  group_by(Study, Site, Year, Month, Date, CageID, Treatment, FishID, Tag, Error, Empty, Fullness, Digestion, FullStomachWeight, EmptyStomachWeight, StomachWeightDifference, TotalContentsWeight, Taxon, TaxaWeight, Comments) %>% 
  summarise(sumcount = sum(Count, na.rm = TRUE))


#check the duplicates

dups2 = dietnewwt%>% 
  group_by(`FishID`, Taxon) %>% 
  summarize(n = n()) %>% 
  filter(n>1)

#good now

#check the fish numbers again to make sure all retained

n2024_2 = dietnewwt %>% 
  group_by(`FishID`, Year) %>% 
  filter(Year == 2024) %>% 
  summarise(n= n())

n2023_2 = dietnewwt %>% 
  group_by(`FishID`, Year) %>% 
  filter(Year == 2023) %>% 
  summarise(n= n())

n2022_2 = dietnewwt %>% 
  group_by(`FishID`, Year) %>% 
  filter(Year == 2022) %>% 
  summarise(n= n())

n2021_2 = dietnewwt %>% 
  group_by(`FishID`, Year) %>% 
  filter(Year == 2021) %>% 
  summarise(n= n())

n2019_2 = dietnewwt %>% 
  group_by(`FishID`, Year) %>% 
  filter(Year == 2019) %>% 
  summarise(n= n())

#all numbers are good. wooooo


#EDI File-----------

#now make a csv for edi, but pretty up the format first which is essentially just removing the taxa weight column and renaming the sum count

dietedicsv = dietnewwt %>% 
  rename(Count = sumcount) %>% 
  ungroup() %>% 
  select(Study:Year, Date:Taxon, Count, TaxaWeight, Comments)


write.csv(dietedicsv, "Outputs/cagediets2019to2024.csv", row.names = FALSE)


  
####################For Analysis to mess with later######################

#Biomass + Group Prey Categories-----
  
#add biomass to the full file and then can filter out for the feeding and summer fall stuff
  
#this also has prey categories in here.
  
#since we don't have lengths for amphipods, I'll need to eventually add average lengths for these lifestages so we can have a weight

#remove most of the identifying taxonomic stuff here

biomass = read.csv("~/Data/Delta Smelt Cage Studies/Analysis/Data/All Cage Biomass Crosswalk 2025- Temp.csv", check.names = FALSE) %>% 
  select(Taxon, `Taxa Group`, `Carbon Weight`)

dietbm = dietedicsv %>% 
  left_join(., biomass, by= "Taxon") %>% 
  mutate(Biomass = Count*`Carbon Weight`)
  
  
#Feeding Study Analysis-----

#2022 data. Analysis for feeding study paper

#want to pivot wide, make an empty or not column, and also want total prey counts
#want to take from the biomass file since this has corrections plus the taxa groups

diet2022_wide= dietbm %>%
  filter(Year == 2022) %>% 
  pivot_wider(id_cols = c(`Study`: `Total Contents Weight`, `Comments`),
              names_from = Taxon, 
              values_from = Count, 
              values_fill = 0) %>% 
  mutate("TotalNumberofPrey"= sum(across(Empty:`Osphranticum labronectum`)) )

diet22empties= diet2022new %>% 
  mutate(Empty = )

#says there are duplicates in some here. Want to check if the  values are truly unique

dups2022= diet2022new%>% 
  group_by(`Fish ID`, Taxon)%>%
  summarize(n = n()) %>% 
  filter(n>1)

diet2022wide = diet2022new%>% 
  pivot_wider(id_cols = c(`Study`: `Total Contents Weight`,`Contents`),
              names_from = Taxon, 
              values_from = Count, 
              values_fill = 0, 
              values_fn = sum)

######################### Summer/Fall Synthesis Analysis--################

#limit it so its just summer/ fall. Applies to 2019

summerdiet = dietnew_long %>% 
  filter(Month %in% c("8", "9", "10"))

#merge the biomass sheet to group categories

###Group Prey------

summergrp = summerdiet %>% 
  left_join(., biomass, by = "Taxon") %>% 
  select(-c(`Life History Stage`, Notes: Species)) %>% 
  filter(`Taxa Group` != "NA") %>% 
  filter(Count != "NA") %>% 
  summarise(group_by(Study, Site, Year, Date, `Fish ID`, Error, Treatment, Cage, Fullness, Digestion, `Total Contents Weight`, Comments,`Taxa Group`), 
            sum = sum(Count))


##Diet by Number----------
  
#make graph with the taxa group by site and flip/ scrub

#pivot wider first to add zeros to prey

sumtotprey = diet_lw%>% 
  filter(`TaxaGroup` != "NA")%>% 
  select(-c(`TaxaGroup`, `Amphipod Taxa Group`)) %>% 
  pivot_wider(names_from = `Prey Taxa LH Stage`, 
              values_from = Count, 
              values_fill = 0) %>% 
  select(-EMPTY) #remove the empty as a prey category

write.csv(totprey, file = "Outputs/cagedietnumber_wide.csv")

#bring back the taxa group from the diet file. Weird way to do this but whatever

taxagrp = read.csv("~/Data/Delta Smelt Cage Studies/Analysis/Data/Cage Biomass crosswalk Updated.csv") %>% 
  select(CageCode, TaxaGroup) %>% #only keep the taxa groupings since doing count first
  filter(CageCode != "") %>% 
  rename("Prey Taxa LH Stage" = CageCode)

#now merge so can do the groupings with the zero prey. first need to pivot longer

#then total by fish

totpreygrp = totprey %>% 
  pivot_longer(cols = `Pseudodiaptomus forbesi copepodid`: `Eurytemora affinis copepodid`,  
               names_to = "Prey Taxa LH Stage", 
               values_to = "Count") %>% 
  merge(., taxagrp) %>% 
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



full_glm = glm(Count~Treatment + Location, data = totprey_long)
summary(full_glm) 
#signif by location, but not treatment


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

bm_conversions = read.csv("~/Data/Delta Smelt Cage Studies/Analysis/Data/Cage Biomass crosswalk Updated.csv") 
  

diet_meso_bm = totprey_long %>% 
  rename("CageCode" = 'Prey Taxa LH Stage') %>% 
  left_join(., bm_conversions, by = "CageCode") %>% 
  filter(`TaxaGroup` != "NA" & `TaxaGroup` != "Other Amphipod" & `CageCode` != "Tanaidacea" & 
           `TaxaGroup` != "Corophiidae" & `TaxaGroup` != "Gammaridea " ) %>% 
  filter(`CageCode` != "Aphididae (winged) adult") %>% 
  mutate(Biomass = Count* Carbon_weight_ug) %>%  #make biomass column 
  select(-c(EMP_Code, Taxlifestage, Notes))


#Zoop & Amphipods------

# need to figure out the biomass for amphipods in the diets 
#don't have lengths from in the diets but do have them from the ones on the cage and in the zoop data
#read that in to see the average length and use that to calculate biomass

amphzoop = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Zooplankton/DWR_CageZoop2023_Complete_TEC_1.22.2024.xlsx", sheet = "Amph Data") %>% 
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

macro_convers = read_xlsx("~/Data/Delta Smelt Cage Studies/Analysis/Data/Mesomicromacro Biomass conversions Aug2024.xlsx", sheet = "Macro-zooplankton") %>% 
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
            tot_fishwetwt = sum(WetWeight_g)) %>% 
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
  mutate("Percent Fullness (%BW)" = ((tot_fishwetwt/LabWeight_g)*100))  #make a column of % GF as a function of body weight. 
  

#calc fullnessboxplots 

#avg by cage--- use this for the boxplot graph

cagecfull = calcfullness %>% 
  group_by(Location, Treatment, `Cage ID`) %>% 
  summarise(avg_cagecf = mean(`Percent Fullness (%BW)`))


#boxplots 

tcf1 = ggplot(cagecfull, aes(x = Treatment, y = `avg_cagecf`, fill = Location))
tcf2 = tcf1 + geom_boxplot() + 
  ylab("Average Calc Fullness (%)")+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))

tcf2

ggsave(plot = tcf2, filename = "Graphs/ Calc Fullness Boxplot.tiff", device = "tiff",width = 6, height =5, units = "in", dpi = 300)

save(calcfullness, file = "calcfull.RData")


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

wet_convers = bm_conversions %>% 
  select(-c(EMP_Code, Taxlifestage))


diet_mesowet = diet_lw %>% 
  rename("CageCode" = 'Prey Taxa LH Stage') %>% 
  left_join(., wet_convers, by = "CageCode") %>% 
  #filter(`Taxa Group` != "NA" & `Taxa Group` != "Amphipod") %>% 
  #filter(`CageCode` != "Aphididae (winged) adult" & `CageCode` !="EMPTY") %>% 
  mutate(WetWeightbyCount_g = Count* WetWeight_g) %>%  #make column for wet weight by the prey count 
  select(-c( Notes, Year, Study))

#add up prey wet weights by taxa group by fish for total gut content weight by fish

totmes_wet = diet_mesowet %>% 
  group_by(Location, Treatment, `Cage ID`, FishID, LabForkLength_mm, LabWeight_g, Fullness, Digestion,`Total Contents Weight`) %>%   summarize(totalwetwt = sum(WetWeightbyCount_g))


avg_cagewet = totmes_wet %>% 
  group_by(Location, Treatment, `Cage ID`, `Taxa Group`) %>% 
  summarise(avg_cagewet_mg = (mean(totalwetwt))*1000)

avg_treatwet = avg_cagewet %>% 
  group_by(Location, Treatment, `Taxa Group`) %>% 
  summarise(avg_treatwet_mg = mean(avg_cagewet_mg))

#boxplot of wet weight

wetwt1 = ggplot(avg_cagewet, aes(x = Treatment, y = `avg_cagewet_mg`, fill = Location))
wetwt2 = wetwt1 + geom_boxplot() + 
  ylab("Average Diet Wet Weight (mg)")

wetwt2



#stacked plots with wet weight




treatwet1 = ggplot(avg_treatwet, aes(x = Treatment, y = avg_treatwet_mg))
treatwet2 = treatwet1 + geom_bar(stat = "identity", aes(fill = `Taxa Group`)) + 
  facet_wrap(facets= vars(Location)) 

treatwet2










