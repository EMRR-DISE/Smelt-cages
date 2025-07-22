#organize all zoop data for data publication

#Want to make it match the format here: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1248.3

library(tidyverse)
library(readxl)

#Dataset on EDI
zoopsEDI = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1248/3/f0a145a59e6659c170988fa6afa3f232")
names(zoopsEDI)
#[1] "Location"          "Date"              "SetTime"           "FlowMeterStart"    "FlowMeterEnd"     
#[6] "Rotations"         "MeshSize"          "RingSize"          "TotalVolume"       "SubsampledVolume" 
#[11] "Taxon"             "Count"             "nSubsamples"       "TaxonomicGrouping" "BiomassIndex"     
#[16] "TotalBiomass"   
#Location = RV or DWSC
#Dates = Jan-Mar 2019
#It's not clear if the "count" is the count when adjusting for subsampling or not. I"m pretty sure it was not.

zoopsEDIed = zoopsEDI %>%
  mutate(Date = mdy(Date),AdjCount = Count*TotalVolume/SubsampledVolume,
         TowDistance = Rotations*57560/999999, VolumeSampled = ((pi*((RingSize/100)^2))/4)*TowDistance,
         CPUE = AdjCount/VolumeSampled, BPUE = CPUE*BiomassIndex, SampleType = "Tow")%>%
  rename(Site = Location) %>%
  select(Site, Date,  Taxon,FlowMeterStart, FlowMeterEnd, Rotations, 
         VolumeSampled, TotalVolume, SubsampledVolume, SampleType,
         Count, AdjCount, CPUE, BPUE)
unique(zoopsEDI)

#So first we need the rest of the 2019 zooplankton

zoops2019 = read_csv("smelt_2019_allseasons/data_clean/zoop_abundance.csv")
names(zoops2019)
#[1] "Location"          "Date"              "Method"            "TowDistance"       "SetTime"          
#[6] "FlowMeterStart"    "FlowMeterEnd"      "Rotations"         "MeshSize"          "RingSize"         
#[11] "TotalVolume"       "SubsampledVolume"  "Taxon"             "Count"             "nSubsamples"      
#[16] "TaxonomicGrouping" "BiomassIndex"      "TotalBiomass"      "RingSize_m"        "distance"         
#[21] "volumesampled"     "samplecount"       "ind_m3"            "samplebiomass"     "bio_m3"           
#[26] "radius"            "Taxa"  

unique(zoops2019$Date)
#crap, this is also just spring

zoops2019all = read_csv("smelt_2019_allseasons/data_raw/ZoopDataCombined_1.25.22_qaqc'ed.csv")
unique(zoops2019all$Date...3)
#nope, that's also jsut spring

zoops2019sumfall = read_csv("smelt_2019_allseasons/data_raw/ZoopData_SummerFall_2019.csv")
unique(zoops2019sumfall$Date)
# names(zoops2019sumfall)
# [1] "Station"        "Date"           "MeshSize"       "RingSize_cm"    "TotalVolume_ml" "Subsampled_ml" 
# [7] "ID"             "Count"          "#ofSubsample"   "Taxa"           "ugC.ind"        "Total.Biomass" 
# [13] "Comments"       "EnteredBy"      "EnteredDate"    "QC'ed by"       "QCDate" 
unique(zoops2019sumfall$Station)
unique(zoops2019sumfall$RingSize_cm)

#need teh effor tinfo;

#bring in tow data
aug <- read_csv("smelt_2019_allseasons/data_raw/Field_Environmental_Data_Aug_qaqc'ed_11.04.20.csv") %>%
  mutate(Set.Time.min = as.numeric(Set.Time.min))
oct <- read_csv("smelt_2019_allseasons/data_raw/Field_Environmental_Data_Oct_qaqc'ed_12.11.20.csv")
effort = bind_rows(aug, oct) %>%
  select(Date, Location, `Zooplankton?`, Start.Time, Start.Meter, End.Meter, Revs, Zoop.Notes) %>%
  mutate(Date = mdy(Date)) %>%
  rename(Site = Location)

zoops2019sumfalled = zoops2019sumfall %>%
  mutate(Site = case_when(Station %in% c("Yolo", "YOLO", "STTD", "STTD-YOLO") ~ "Yolo",
                              TRUE ~ Station),
         Date = mdy(Date),
         RingSize_cm = 50) %>%
  left_join(effort) %>%
  rename(Taxon = ID, Time = Start.Time,FlowMeterStart = Start.Meter, 
         FlowMeterEnd = End.Meter, Rotations = Revs, TotalVolume = TotalVolume_ml, SubsampledVolume=Subsampled_ml) %>%
  mutate( SampleType = "Tow", TowDistance = Rotations*57560/999999, 
          VolumeSampled = ((pi*((RingSize_cm/100)^2))/4)*TowDistance,
          AdjCount = Count*TotalVolume/SubsampledVolume, CPUE = AdjCount/VolumeSampled)%>%
  select(Site, Date,  Taxon,FlowMeterStart, FlowMeterEnd, Rotations, 
         VolumeSampled, TotalVolume, SubsampledVolume, SampleType,
         Count, AdjCount, CPUE)

#they have a differen subsample volume for big thingsa nd small things. Gross. 

test = filter(zoops2019sumfalled, Taxon == "Rotifers")

zoops2023 = read_excel("biofouling/ICF_zoops_data_cages.xlsx", sheet = "Zoop Data")
names(zoops2023)
# [1] "Location"            "Date"                "Time"                "Site"                "ID'd By"            
# [6] "ID Date"             "Sample Volume (mL)"  "Pipette Volume (mL)" "# of Subsamples"     "Species Name"       
# [11] "Count"               "Sum Count"           "Photos"              "Comments" 
unique(zoops2023$Site)
zoops2023ed = zoops2023 %>%
  rename(Cage = Location, Taxon = `Species Name`,
         TotalVolume = `Sample Volume (mL)`,  SubsampledVolume = `# of Subsamples`) %>%
  mutate(VolumeSampled = 0.0378541, #ten gallons is 0.0378 cubic meters
         SampleType = "Pump",  AdjCount = Count*TotalVolume/SubsampledVolume, CPUE = AdjCount/VolumeSampled)%>%
  select(Site, Date,Cage, Taxon,VolumeSampled, TotalVolume, SubsampledVolume, SampleType,
         Count, AdjCount, CPUE)


zoops2024 =  read_excel(("smelt_2024_SMSCGs/DWR_CageZoop_2024_Complete_TEC_12.10.24_corrected.xlsx"), sheet = "Zoop Data")
names(zoops2024)
# [1] "Location"            "Date"                "Time"                "Site"                "ID'd By"            
# [6] "ID Date"             "Sample Volume (mL)"  "Pipette Volume (mL)" "# of Subsamples"     "Species Name"       
# [11] "Count"               "Sum Count"           "Photos"              "Comments"      
unique(zoops2024$Site)
zoops2024ed = zoops2024 %>%
  rename(Cage = Location,  Taxon = `Species Name`,
         TotalVolume = `Sample Volume (mL)`,  SubsampledVolume = `# of Subsamples`) %>%
  mutate(VolumeSampled = 0.0378541, #ten gallons is 0.0378 cubic meters
         SampleType = "Pump", AdjCount = Count*TotalVolume/SubsampledVolume, CPUE = AdjCount/VolumeSampled) %>%
  select(Site, Date,  Cage, Taxon,VolumeSampled, TotalVolume, SubsampledVolume, SampleType,
         Count, AdjCount, CPUE)

#put them all together
allzoops = bind_rows(zoopsEDIed, zoops2019sumfalled, zoops2024ed, zoops2023ed) %>%
  filter(!is.na(Count)) %>%
  select(-BPUE)


#export taxon names


#clean it up a bit
allzoops = mutate(allzoops, Taxon = str_remove(Taxon, " spp"),
                  Taxon = str_remove(Taxon, "\\."),
                         Taxon = str_remove(Taxon, " sp"),
                         Taxon = str_remove(Taxon, " UNID"),
                  Taxon = str_remove(Taxon, "UNID "),
                  Taxon = str_replace(Taxon, "copepodite", "copepodid"),
                  Taxon = str_replace(Taxon, "Gastropod", "Gastropoda"),
                  Taxon = str_replace(Taxon, "Gastropodaa", "Gastropoda"),
                  Taxon = str_replace(Taxon, "Baby Clam", "Bivalvia"),
                  Taxon = str_replace(Taxon, "Clam", "Bivalvia"),
                  Taxon = str_replace(Taxon, "Bivalve", "Bivalvia"),
                  Taxon = str_replace(Taxon, "Baby snail", "Gastropoda"),
                  Taxon = str_replace(Taxon, "Barnacles", "Barnacle nauplii"),
                  Taxon = str_replace(Taxon, "Snail", "Gastropoda"),
                  Taxon = str_replace(Taxon, "Bivalve veliger", "Bivalvia veliger"),
                  Taxon = str_replace(Taxon, "Rotifers", "Rotifer"),
                  Taxon = str_replace(Taxon, "Calanoid", "Calanoida"),
                  Taxon = str_replace(Taxon, "Cyclopoid", "Cyclopoida"),
                  Taxon = str_replace(Taxon, "Eurytemora nauplii", "Eurytemora affinis nauplii"),
                  Taxon = str_replace(Taxon, "Harpacticoid", "Harpacticoida"),
                  
                  Taxon = str_replace(Taxon, "Harpaticoida", "Harpacticoida"),
                  Taxon = str_replace(Taxon, "Harpacticoidas", "Harpacticoida"),
                  Taxon = str_replace(Taxon, "Harpaticoid", "Harpacticoida"),
                  Taxon = str_replace(Taxon, "Insect ", "Insecta "),
                  Taxon = str_replace(Taxon,  "Ostracod", "Ostracoda"),
                  Taxon = str_replace(Taxon, "Ostracodas", "Ostracoda"),
                  Taxon= str_replace(Taxon, "Annelid", "Annelida"),
                  Taxon = str_replace(Taxon, "Annelidaa", "Annelida"),
                  Taxon = str_replace(Taxon, "Amphipod", "Amphipoda"),
                  Taxon = str_replace(Taxon, "Mysids", "Mysid"),
                  Taxon = str_replace(Taxon, "chitoni", "chiltoni"),
                  Taxon = str_replace(Taxon,"Scaphloberis", "Scapholeberis"),
                  Taxon = str_replace(Taxon, "Tropocylcops", "Tropocyclops"),
                  Taxon = str_replace(Taxon, "Amphipodaa", "Amphipoda"),
                  Taxon = str_replace(Taxon, "affinis", "carolleeae"),
                  Taxon = str_replace(Taxon, "Acanthocyclops vernalis", "Acanthocyclops"),
                  Taxon = str_replace(Taxon, "Copepodid nauplii", "Copepod nauplii"),
                  Site = case_match(Site, "Montezuma" ~ "Suisun Marsh",
                                        "SM" ~ "Suisun Marsh",
                                        "RV" ~ "Rio Vista",
                                    "Rio Vista" ~ "Rio Vista",
                                        "DWSC" ~ "SDWSC",
                                        "Yolo" ~ "Yolo Bypass",
                                    .default = Site),
                  Study = NA,
                  Study = case_when(Date < ymd("2019-4-1") ~ "Enclosure Prototype",
                                    is.na(Study) & Date < ymd("2019-9-1") ~ "North Delta Pilot",
                                    is.na(Study) & Date < ymd("2019-11-10") ~ "Suisun Marsh Pilot",
                                    is.na(Study) & Date < ymd("2023-12-10") ~ "Biofouling Study",
                                    TRUE ~ "SMSCG"))


allzoops = select(allzoops, Study, Site, Date, Cage, SampleType,
                  FlowMeterStart, FlowMeterEnd, Rotations, VolumeSampled,
                  TotalVolume, SubsampledVolume, Taxon, Count, AdjCount, CPUE) %>%
  rename(Enclosure = Cage)


##### feeding study data ######################

#I missed some data

feedzoop1 = read_csv("data/RIO VISTA_2022-02-08_150_SMELT STUDY.csv")
feedzoop2 = read_csv("data/RIO VISTA_2022-02-10_150_SMELT STUDY.csv")
feedzoop3 = read_csv("data/RIO VISTA_2022-02-16_150_SMELT STUDY.csv")
feedzoop4 = read_csv("data/RIO VISTA_2022-03-01_150_SMELT STUDY.csv")
feedzoop5 = read_csv("data/RIO VISTA_2022-03-09_150_SMELT STUDY.csv")
feedzoop6 = read_csv("data/RIO VISTA_2022-02-22_150_SMELT STUDY.csv")
feedzoop = bind_rows(feedzoop1, feedzoop2, feedzoop3, feedzoop4, feedzoop5, feedzoop6) %>%
  mutate(Study = "Feeding Study", Site = "Rio Vista", SampleType = "Tow") %>%
  rename(Date = date, Count = count)

feed_env = read_excel("data/FeedingStudy_Environmental.xlsx", sheet = "Zoop")
#need the low flow flowmeter constant


#CPUE = C*(vsample/vsub)/Vnet
#vnet = (flowmeterend-flowmeterstart)*57560/999999*pi*r2
#still need r, but closer
feed_env = mutate(feed_env, Volume = (MeterStart-MeterEnd)*57560/999999*pi*.5*.5/4) %>%
  rename(FlowMeterStart = MeterStart, FlowMeterEnd = MeterEnd, Rotations = MeterCheck) %>%
  select(Date, FlowMeterStart, FlowMeterEnd, Rotations, Volume)

feedzoops = left_join(feedzoop, feed_env) %>%
  mutate(SubsampledVolume = case_when(category == "MICROZOOPLANKTON & NAUPLII" ~ sub2_ml,
                                      TRUE ~ sub1_ml),
         AdjCount = Count*(v1_ml/SubsampledVolume),
         CPUE = Count*(v1_ml/SubsampledVolume)/Volume) %>%
  rename(VolumeSampled = Volume, TotalVolume = v1_ml, Taxon = taxon)%>%
  select(Study, Site, Date, SampleType, FlowMeterStart, FlowMeterEnd, Rotations, 
         VolumeSampled, TotalVolume, SubsampledVolume, Taxon, Count, AdjCount, CPUE) %>%
  rename(feedtaxa = Taxon)

write.csv(unique(feedzoops$Taxon), "data/feedzoopstaxa.csv")
feedcrosswalk = read_csv("data/cagezooptaxa_feedstudycrosswalk.csv") %>%
  filter(!is.na(feedtaxa))

feedzoops = left_join(feedzoops, feedcrosswalk) %>%
  mutate(Taxon = case_when(is.na(Taxon) ~ "Rotifer",
                           TRUE ~ Taxon)) %>%
  select(-feedtaxa) %>%
  group_by(Study, Site, Date, SampleType, FlowMeterStart, FlowMeterEnd, Rotations,
           VolumeSampled, TotalVolume, SubsampledVolume, Taxon) %>%
  summarise(Count = sum(Count), AdjCount = sum(AdjCount), CPUE = sum(CPUE)) %>%
  filter(CPUE !=0)

allzoops = bind_rows(allzoops, feedzoops) %>%
  filter(CPUE !=0)

#############################################################

write.csv(allzoops, "data/allcagezoops.csv", row.names = F)
save(allzoops, file = "data/allcagezoops.RData")

bugslookup = read.csv(("data/cagezooptaxa_lookuptable.csv"))

zooptax = sort(unique(allzoops$Taxon))

#compare to the diet critters

diet = read_csv("data/Delta Smelt Cage Diets 2019-2024.csv")

dietx = diet %>%
  rename(Taxon = `Prey Taxa LH Stage`, Cage = `Cage ID`) %>%
  mutate( Taxon = str_replace(Taxon, "Harpaticoida","Harpacticoida"),
               Taxon = str_replace(Taxon, "Diaphonosoma", "Diaphanosoma"),
          Taxon = str_replace(Taxon, "affinis", "carolleeae"),
          Taxon = str_replace(Taxon, "Acanthocyclops vernalis", "Acanthocyclops"),
               Taxon = str_replace(Taxon, "Gammaridea", "Gammaridae"),
          Site = case_match(Site, "Montezuma" ~ "Suisun Marsh",
                                "RV" ~ "Rio Vista",
                                "RIVERS" ~ "Rio Vista",
                                "High RVERS"~ "Rio Vista",
                                "Low RVERS" ~ "Rio Vista",
                            "Sac DWSC" ~ "SDWSC",
                            .default = Site),
          Site = case_when(is.na(Site) & substr(Tag, 1, 2) == "RV" ~ "Rio Vista",
                           is.na(Site) & substr(Tag, 1, 2) == "SM" ~ "Suisun Marsh",
                           is.na(Site) & substr(Tag, 1, 2) == "YB" ~ "Yolo Bypass",
                           TRUE ~ Site),
          Study = case_match(Study, "Biofouling" ~ "Biofouling Study",
                             "Feeding" ~ "Feeding Study",
                             "DSM" ~ "Domestication/Behavior",
                             .default = Study),
          Study = case_when(is.na(Study) & Date %in% c("2019-11-06","2019-11-07") ~ "Suisun Marsh Pilot",
                            is.na(Study) & Date %in% c("2019-08-28") ~ "North Delta Pilot",
                            TRUE ~ Study)) %>%
  select(-`Prey Taxa`, -`LH Stage`, -`Month`)


write.csv(dietx, "data/allcagediet.csv", row.names = F)
save(dietx, file = "data/allcagediet.RData")


#now the biofouling bugs

bugs2023 = read_excel("biofouling/ICF_zoops_data_cages.xlsx", sheet = "Amph Data")
names(bugs2023)

unique(bugs2023$Site)
bugs2023ed = bugs2023 %>%
  rename(Cage = Location,  Taxon = `Species`) %>%
  mutate(SampleType = "quadrat",  CPUE = count)%>%
  select(Site, Date,Cage, Taxon, SampleType,
         count, CPUE,  pc_frag_mut, a_nr_g, m_1:m_50)

bugs2023ed$meanlength = rowMeans(bugs2023ed[,c(10:59)], na.rm =T)

bugs2023sum = group_by(bugs2023ed, Site, Date, Cage, Taxon) %>%
  summarize(Count = sum(count), CountWhole = sum(count[which(is.na(pc_frag_mut))]), fragments = sum(count[which(!is.na(pc_frag_mut))]),
            Meanlength = mean(meanlength, na.rm =T))


bugs2024 =  read_excel(("smelt_2024_SMSCGs/DWR_CageZoop_2024_Complete_TEC_12.10.24_corrected.xlsx"), sheet = "Amph Data")
names(bugs2024)
bugs2024ed = bugs2024 %>%
  rename(Cage = Location, Taxon = `Species`) %>%
  mutate(SampleType = "quadrat",  CPUE = count)%>%
  select(Site, Date,Cage, Taxon, SampleType,
         count, CPUE,  pc_frag_mut, a_nr_g, m_1:m_50)

bugs2024ed$meanlength = rowMeans(bugs2024ed[,c(10:59)], na.rm =T)

bugs2024sum = group_by(bugs2024ed, Site, Date, Cage, Taxon) %>%
  summarize(Count = sum(count), CountWhole = sum(count[which(is.na(pc_frag_mut))]), fragments = sum(count[which(!is.na(pc_frag_mut))]),
            Meanlength = mean(meanlength, na.rm =T))

allbugs = bind_rows(bugs2024sum, bugs2023sum) %>%
  mutate(Taxon = str_remove(Taxon, " spp"),
         Taxon = str_remove(Taxon, " UNID"),
         Taxon = str_replace(Taxon, " Copepodid", " copepodid"),
         Taxon = str_replace(Taxon, " Zoea", " zoea"),
         Taxon = str_replace(Taxon, "Gastropod", "Gastropoda"),
         Taxon = str_replace(Taxon, "Ostracod", "Ostracoda"),
         Taxon = str_replace(Taxon, "Amphipod", "Amphipoda"),
         Taxon = str_replace(Taxon, "Cyclopoid copepodid", "Cyclopoida copepodid"),
         Taxon = str_replace(Taxon, "Cumacea", "Cumacean"),
         Site = case_match(Site, "Montezuma" ~ "Suisun Marsh",
                               "Rio Vista" ~ "Rio Vista"),
         Study = case_when(year(Date) == 2023 ~ "Biofouling Study",
                           year(Date) == 2024 ~ "SMSCG"))
unique(allbugs$Taxon)

unique(allbugs$Taxon) %in% bugslookup$Taxon

unique(allbugs$Taxon)[which(!unique(allbugs$Taxon) %in% bugslookup$Taxon)]


write.csv(allbugs, "data/BiofoulingBugs.csv", row.names = F)


###################################################################
#algae data


#add algal biomass
algae<-read_csv("biofouling/Algae_biomass.csv")

#Convert biomass to milligrams to match the other measurements
algae = rename(algae, Site = Station, InsideOutside = Sample_Location, Notes = `Lab notes`) %>%
  mutate(AlgalBiomass_mg = Dry_Weight*1000, Cage = paste("Cage", Cage),  Date = mdy(Date)) %>%
  select(Site, Treatment, Date,  Cage,  InsideOutside, AlgalBiomass_mg, Algae_color,Notes) %>%
  mutate(Site = case_when(Site == "Rio_Vista" ~ "Rio Vista",
                          Site == "Montezuma" ~ "Suisun Marsh"))
write.csv(algae, "data/AlgalBiomass.csv", row.names = F)
