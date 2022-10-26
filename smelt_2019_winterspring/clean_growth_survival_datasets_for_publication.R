
library(here)
library(dplyr)

Cagetype_data <- read.csv(here::here("smelt_2019_winterspring", "data_clean", "Cagetype_data.csv")) %>%
  mutate(Delta_Weight = Post_Weight_g - Pre_Weight_g,
         PropChange_Weight = round(Delta_Weight/Pre_Weight_g,2),
         Delta_FL = Post_FL_cm - Pre_FL_cm,
         PropChange_FL = round(Delta_FL/Pre_FL_cm,2),
         Site = factor(Site, levels = c("RV", "DWSC")),
         Enclosure = ifelse(Mesh == "wrap", "A", ifelse(Mesh == "large", "B", ifelse(Mesh == "small", "C", ifelse(Mesh == "control", "control", NA)))))%>%
  select(Site, Cage, Enclosure, Mesh, Pre_FL_cm, Post_FL_cm, PropChange_FL, Pre_Weight_g, Post_Weight_g, PropChange_Weight, Pre_CF, Post_CF, Delta_CF) %>%
  filter(Mesh!= "control")

write_csv(Cagetype_data, here::here("smelt_2019_winterspring", "data_clean", "2019_smeltstudy_growth-cf_data.csv"))

# See EDI table for survival in manuscript_weight_length_k_survival.Rmd
