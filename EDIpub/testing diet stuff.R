#do some quick chekin on the diet data

library(tidyverse)
library(RColorBrewer)
library(vegan)

diets = read_csv("EDIpub/cagediets2019to2024.csv")

ggplot(diets, aes(x = Study, y = Fullness)) + geom_boxplot()



ggplot(diets, aes(x = Study, y = StomachWeightDifference)) + geom_boxplot()+
  coord_cartesian(ylim = c(0,.1))

ggplot(diets, aes(x = TotalContentsWeight, y = StomachWeightDifference)) + geom_point() 

ggplot(diets, aes(x = Study, y = Count, fill = Taxon))+ geom_col()+theme(legend.position = "none")


ggplot(diets, aes(x = FishID, y = Count, fill = Taxon))+ geom_col()+theme(legend.position = "none")+
  facet_wrap(~Study, scales = "free")
test = filter(diets, Count>100)
test = filter(diets, Study == "Feeding Study", Enclosure == "FCCL Control")
test = filter(diets, Study == "Feeding Study")

ggplot(diets, aes(x = FishID,  fill = Taxon))+ geom_bar()+theme(legend.position = "none")+
  facet_wrap(~Study, scales = "free")

zeros = filter(diets, Count ==0)
errors = filter(diets, Error)

ggplot(diets, aes(x = Date)) + geom_bar()+
  facet_wrap(~Study, scales ="free")

fish = read_csv("data/cagemetrics.csv")

test = select(diets, Study, FishID, CageID, Date, Tag) %>%
  distinct()

test2 = left_join(test, fish, by = "FishID")

missing = test[which(!test$FishID %in% fish$FishID),]
missing2 = test[which(!test$Tag %in% fish$Tag),]


dosbev = filter(fish, Study == "Domestication/Behavior")
write.csv(missing, "Dietsthatdontmatchfish.csv", row.names = F)


###suisun cages #################


mypal = c(brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

bugslookup = read.csv(("data/cagezooptaxa_lookuptable_analysis.csv"))
suisun = filter(diets, Study %in% c("SMSCG", "Suisun Marsh Pilot", "Biofouling Study"), Site != "Yolo Bypass") %>%
  left_join(bugslookup) %>%
  filter(!is.na(Analy))

#plot for report
ggplot(suisun, aes(x = Site, y = Count, fill = Analy)) + geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~Year)+ scale_fill_manual(values = mypal, name = "Taxon")+ ylab(NULL)+ theme_bw()

ggsave("plots/suisunvRV_diets.tiff", width =7, height =5)

#set it up for a PERMANOVA
bugsmat = suisun %>%
  pivot_wider(id_cols = c(Study, Site, Year, Date, Enclosure, Treatment, FishID, Empty),
                      names_from = Analy, values_from = Count, values_fill = 0, values_fn = sum)
bugsmat2 = as.matrix(select(bugsmat, Amphipoda:last_col()))
envmat = select(bugsmat, Study, Site, Year, Date, Enclosure, Treatment, FishID, Empty)[which(rowSums(bugsmat2) !=0),]
bugsmat2 = bugsmat2[which(rowSums(bugsmat2) !=0),]

a1 = adonis2(bugsmat2 ~ Site*Year, data = envmat)
a1
a2 = adonis2(bugsmat2 ~ Site+Year, data = envmat)
a2   

a2 = adonis2(bugsmat2 ~ Site, data = envmat)
a2   
