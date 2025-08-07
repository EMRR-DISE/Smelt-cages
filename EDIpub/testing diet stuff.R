#do some quick chekin on the diet data

library(tidyverse)

diets = read_csv("data/cagediets2019to2024.csv")

ggplot(diets, aes(x = Study, y = Fullness)) + geom_boxplot()



ggplot(diets, aes(x = Study, y = StomachWeightDifference)) + geom_boxplot()+
  coord_cartesian(ylim = c(0,.1))

ggplot(diets, aes(x = TotalContentsWeight, y = StomachWeightDifference)) + geom_point() 

ggplot(diets, aes(x = Study, y = Count, fill = Taxon))+ geom_col()+theme(legend.position = "none")


ggplot(diets, aes(x = FishID, y = Count, fill = Taxon))+ geom_col()+theme(legend.position = "none")+
  facet_wrap(~Study, scales = "free")
test = filter(diets, Count>100)
test = filter(diets, Study == "Feeding Study", CageID == "FCCL Control")
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
