library(rstatix)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(vegan)
library(readxl)

setwd("~/Delta Smelt/biofouling coding")

# Read in Cage Data -------------------------------------------------------
zoops = read_excel("ICF_zoops_data_cages.xlsx",
                   sheet = "Zoop Data") 

zoops$Total_Count <- (zoops$`Sample Volume (mL)`/(zoops$`Pipette Volume (mL)`*zoops$`# of Subsamples`))*zoops$Count

#Get data ready to be visualized with our naming conventions
treatments = data.frame(Location = c("Cage 1", "Cage 2", "Cage 3", "Cage 4",
                                     "Cage 5", "Cage 6", "Cage 7", "Cage 8"),
                        Treatment = c("Exchange", "Scrub", "Exchange", "Scrub",
                                      "Exchange", "Scrub", "Exchange", "Scrub"))

zoops = mutate(zoops, InOut = case_when(str_detect(Location, "Outside") ~ "Outside",
                                        str_detect(Location, "Inside") ~ "Inside"),
               CageNum = str_sub(Location, 1, 6)) %>%
  left_join(treatments, by = c("CageNum" = "Location")) %>%
  mutate(Treatment = case_when(InOut == "Outside" ~ "Outside",
                               TRUE ~ Treatment),
         Week = week(Date),
         SampleID = paste(CageNum, Date, InOut))
#Adjust for subsampling
zoops$Total_Count <- (zoops$`Sample Volume (mL)`/(zoops$`Pipette Volume (mL)`*zoops$`# of Subsamples`))*zoops$Count
# Read in Diet Data --------------------------------------------------------

diets = read_excel("DSM Cage Diets 2019-2023.xlsx", sheet = "2023")
#assemblages by count
ggplot(diets, aes(x = Tag, y = Count, fill = `Prey Taxa`)) + geom_col()+
  facet_wrap(Treatment~Site, scales = "free_x")
#assemblages by relative abundance
ggplot(diets, aes(x = Tag, y = Count, fill = `Prey Taxa`)) + geom_col(position = "fill")+
  facet_wrap(Treatment~Site, scales = "free_x")
#get weight information from diets ready
weights = group_by(diets, Treatment, Site, Tag) %>%
  summarize(Weight = mean(`Total Contents Weight`))
#Weight of stomach contents of each individual
ggplot(weights, aes(x = Tag, y = Weight)) + geom_col()+
  facet_wrap(Treatment~Site, scales = "free_x")
#Weight of stomach contents by site and treatment
ggplot(weights, aes(x = Site, y = Weight)) + geom_boxplot()+
  facet_wrap(~Treatment, scales = "free_x")

# Read in station and amphipod data ----------------------------------------------------
stations <- read.csv("SMSCG_CBNet_2018to2023CPUE_15Jul2024.csv")
interest <- c("606","609","610","605","MONT","706","704")
stn <- data.frame()
for (i in 1:length(interest)) {
  a <- interest[i]  
  b <- stations[which(stations$Station == a),]
  stn <- rbind(stn, b)
}
stn2023 <- stn[which(stn$Year == 2023),]

stn_long <- gather(stn2023, key = 'Prey', value = 'CPUE', ACARTELA:CUMAC)
#Probably need to filter down dates because it includes the full year

amphipods = read_excel("ICF_zoops_data_cages.xlsx",
                       sheet = "Amph Data")
amphipods = left_join(amphipods, treatments)

# Crosswalk diet and zoop data --------------------------------------------

#Use crosswalk list Rosie made
crosswalk = read_csv("crosswalk_wstns.csv")

#Bring in biomass crosswalk
biomass <- read.csv("Cage Biomass crosswalk1.csv")

masscrosswalk <- inner_join(crosswalk, biomass, by = c(Zooplankton = "CageCode"))

a <- sort(unique(zoops$`Species Name`))
b <- sort(unique(masscrosswalk$Zooplankton))
taxa <- cbind(a,b) #four are missing from IFC data

zoops2 = left_join(zoops, select(masscrosswalk, Zooplankton, Analy, Carbon_weight_ug), by = c("Species Name" = "Zooplankton") )

zoops2.1 = group_by(zoops2, Location, Analy, Site, Date, CageNum, Treatment, SampleID) %>%
  summarize(Mass = sum((Total_Count/0.0378541)*Carbon_weight_ug, na.rm =T), CPUE = sum(Total_Count/0.0378541)) %>%
  mutate(Type = "Zooplankton") #CPUE*Biomass catch/cubic meter

test <- filter(zoops2.1, Analy=="Insect")

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "tomato", "darkgreen", "yellow", "purple")
ggplot(filter(zoops2.1, Treatment != "Outside"), aes(x = Type, y = Mass, fill = Analy)) + geom_col(position = "fill") +
  facet_wrap(Site~Treatment) +
  scale_fill_manual(values = mypal)

ggplot(filter(zoops2.1, Treatment != "Outside"), aes(x = Type, y = CPUE, fill = Analy)) + geom_col(position = "fill") +
  facet_wrap(Site~Treatment) +
  scale_fill_manual(values = mypal)

#Total abundance box plots
ggplot(zoops2.1, aes(x=Treatment, y=CPUE, fill=Site)) +
  geom_boxplot()

ggplot(zoops2.1, aes(x=Treatment, y=CPUE, fill=Site)) +
  geom_boxplot()


# Including FMWT data -----------------------------------------------------
#Data was read in above, dates should be filtered down to what is relevent

stn2 = left_join(stn_long, select(masscrosswalk, EMP_Code, Analy, Carbon_weight_ug), by = c("Prey" = "EMP_Code"))

stn2.1 = group_by(stn2, Year, Station, Date, Analy, CPUE) %>%
  summarize(Mass = sum((CPUE*Carbon_weight_ug), na.rm =T), CPUE = sum(CPUE)) %>%
  mutate(Type = "Station")

ggplot(stn2.1, aes(y=CPUE, fill=Station)) +
  geom_boxplot()

ggplot(stn2.1, aes(y=Mass, fill=Station)) +
  geom_boxplot()


# Statistics --------------------------------------------------------------
species <- unique(zoops2.1$Analy)
locations <- unique(zoops2.1$Location)
dates <- sort(unique(zoops2.1$Date))

#Making a distance matrix
zoopmont1 <- list()
zoopmont2 <- list()
zoopmont3 <- list()
zooprio1 <- list()
zooprio2 <- list()
zooprio3 <- list()

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[1])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zoopmont1[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[3])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zoopmont2[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[5])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zoopmont3[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[2])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zooprio1[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[4])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zooprio2[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

for (i in 1:length(locations)) {
  a <- which(zoops2.1$Location==locations[i] & zoops2.1$Date==dates[6])
  for (j in 1:length(a)) {
    b <- zoops2.1[a,]
    sampleID <- as.character(b[1,1:2])
    zooprio3[[i]] <- cbind(b$`Analy`, b$Mass, sampleID)
  }
}

sampleIDs <- read.csv("SampleIDs.csv")
zoopdistmat <- matrix(ncol=length(species)+1, nrow=length(sampleIDs[,1]))
colnames(zoopdistmat) <- c("SampleID", species)
zoopdistmat[,1] <- sampleIDs[,1]

for (j in 8:14) {
  a <- zoopmont1[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j-7,b] <- a[i,2]
  }
}
for (j in 8:14) {
  a <- zoopmont2[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j,b] <- a[i,2]
  }
}
for (j in 8:14) {
  a <- zoopmont3[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j+7,b] <- a[i,2]
  }
}
for (j in 1:7) {
  a <- zooprio1[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j+21,b] <- a[i,2]
  }
}
for (j in 1:7) {
  a <- zooprio2[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j+28,b] <- a[i,2]
  }
}
for (j in 1:7) {
  a <- zooprio3[[j]]
  for (i in 1:length(a[,1])) {
    b <- which(a[i,1] == colnames(zoopdistmat))
    zoopdistmat[j+35,b] <- a[i,2]
  }
}

class(zoopdistmat) <- "numeric"
str(zoopdistmat)
zoopdistdf <- as.data.frame(zoopdistmat)
rownames(zoopdistdf) <- sampleIDs[,1]
zoopdistdf <- zoopdistdf[,-1]

braydistzoops <- vegdist(zoopdistdf, na.rm=T, method="bray")

nmds <- metaMDS(braydistzoops)

scores(nmds) %>%
  as_tibble(rownames == "Group") %>%
  ggplot(aes(x=NMDS1, y=NMDS2)) +
  geom_point()

##Making NMDS nice plots
metadata <- read.csv("SampleIDs.csv")
braydistzoops <- vegdist(zoopdistdf, na.rm=T, method="bray")

set.seed(1234)
nmds <- metaMDS(braydistzoops)
stressplot(nmds)


scores(nmds) %>%
  as_tibble(rownames = "SampleIDs") %>%
  inner_join(., metadata, by="SampleIDs") %>%
  ggplot(aes(x=NMDS1, y=NMDS2, color=Site)) +
  geom_point()


###running permanova
all_dist <- as.matrix(braydistzoops) %>%
  as_tibble()

all_dist <- cbind(colnames(all_dist), all_dist)
colnames(all_dist) <- c("SampleIDs", all_dist[,1])

meta_distance <- inner_join(all_dist, metadata, by="SampleIDs")

final_dist <- meta_distance %>%
  select(all_of(.[["SampleIDs"]])) %>%
  as.dist()

set.seed(1234)
adonis2(final_dist ~ Treatment, data=meta_distance)

adonis2(final_dist ~ Site, data=meta_distance, permutations = 10000)

all_test <- adonis2(final_dist ~ Treatment*Site, data=meta_distance, permutations = 10000)
meta_distance %>% count(Site)

scores(nmds) %>%
  as_tibble(rownames = "SampleIDs") %>%
  inner_join(., metadata, by="SampleIDs") %>%
  ggplot(aes(x=NMDS1, y=NMDS2, color=Site, fill=Site)) +
  stat_ellipse(geom="polygon", type="norm", level=0.75, alpha=0.2, show.legend = T) +
  geom_point()

scores(nmds) %>%
  as_tibble(rownames = "SampleIDs") %>%
  inner_join(., metadata, by="SampleIDs") %>%
  ggplot(aes(x=NMDS1, y=NMDS2, color=Treatment, fill=Treatment)) +
  stat_ellipse(geom="polygon", type="norm", level=0.75, alpha=0.2, show.legend = T) +
  geom_point()

