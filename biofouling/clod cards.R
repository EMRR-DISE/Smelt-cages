#graph test clod card data (because I can!)

library(tidyverse)
library(lubridate)
library(readxl)

clods = read_excel("data/clod cards.xlsx")

clods = mutate(clods, diff = `Starting Weight`-`Ending weight`) %>%
  filter(treatment != "not used")

ggplot(clods, aes(x = treatment, y = diff))+ geom_boxplot()
ggplot(clods, aes(x = treatment, y = diff, fill =as.factor(`Card #`)))+ 
  geom_col(position = "dodge", color = "black")+
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  ylab("Difference in Weight (g)")+
  theme_bw()
