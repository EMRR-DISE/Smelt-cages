# Look at summary stats of Cage Data
# 5/2/19
# Catarina Pien

rm(list=ls(all=TRUE))
library(tidyverse)

# Load data --------------------------------------------
DWSC <- read.csv("smelt_2019_winterspring/data_raw/DWSC_Edited_Pre-Post_Data.csv")
DWSC$Site <- "DWSC"
RVR <- read.csv("smelt_2019_winterspring/data_raw/RVERS_Edited_Pre-Post_Data.csv")
RVR$Site <- "RV"
FCCL <- read.csv("smelt_2019_winterspring/data_raw/FCCL_Weight_Length_CF.csv")
FCCL_ed <- FCCL %>%
  select(c(1,4,5,6,9,10,11,12,13))
Cage1 <- rbind(DWSC, RVR)
Cage2 <- Cage1 %>%
  select(-c(Tag, Pre_post_Match))
Cage <- rbind(Cage2, FCCL_ed) %>%
  filter(Cage != "FC1") %>%
  filter(Cage != "FC2")

# Add cage type in --------------------------------------
Cage_type <- as.data.frame(matrix(0, ncol = 2, nrow = 8))
colnames(Cage_type) <- c("Cage", "Mesh")

Cage_type$Cage <- c("A", "B", "C", "D", "E", "F", "FCCL1", "FCCL2")
Cage_type$Mesh <- c("wrap", "large", "small", "small", "wrap", "large", "control", "control")

Cagetype_data <- merge(Cage, Cage_type, by = "Cage", all.x = TRUE)

# Write files
# write.csv(Cagetype_data, "CSV_Output/Cagetype_data.csv")

# Reorganize data -------------------------------
Cage$Site <- ordered(Cagetype_data$Site, levels = c("RV", "DWSC"))
Cage$Delta_Wt <- Cage$Post_Weight_g-Cage$Pre_Weight_g
Cage$Site <- ordered(Cage$Site, levels = c("RV", "DWSC"))

# Rearrange data to be able to look at pre-post differences 
Cage_FL <- pivot_longer(Cagetype_data, cols = c(Pre_FL_cm, Post_FL_cm), names_to = "Pre_Post", 
                        values_to = "FL" ) # for FL
Cage_Weight <- pivot_longer(Cagetype_data, cols = c(Pre_Weight_g, Post_Weight_g), names_to = "Pre_Post", 
                            values_to = "Weight" )
Cage_CF <- pivot_longer(Cagetype_data, cols = c(Pre_CF, Post_CF), names_to = "Pre_Post", 
                        values_to = "CF")

# Order Pre_Post variables so "pre" shows up first
Cage_FL$Pre_Post <- ordered(Cage_FL$Pre_Post, levels = c("Pre_FL_cm", "Post_FL_cm"))
levels(Cage_FL$Pre_Post) <- c("Pre-deployment", "Post-deployment")

Cage_Weight$Pre_Post <- ordered(Cage_Weight$Pre_Post, levels = c("Pre_Weight_g", "Post_Weight_g"))
levels(Cage_Weight$Pre_Post) <- c("Pre-deployment", "Post-deployment")

Cage_CF$Pre_Post <- ordered(Cage_CF$Pre_Post, levels = c("Pre_CF", "Post_CF"))
levels(Cage_CF$Pre_Post) <- c("Pre-deployment", "Post-deployment")

### Manuscript Table -----------------------------------------------------------

SumTable <- Cagetype_data %>%
  group_by(Site, Mesh) %>%
  drop_na(Pre_CF) %>%
  summarize(min.pre.fl = min(Pre_FL_cm) * 10,
            max.pre.fl = max(Pre_FL_cm) * 10,
            min.pre.Wt = min(Pre_Weight_g),
            max.pre.Wt = max(Pre_Weight_g), 
            n = n(),
            mean.pre.fl = round(mean(Pre_FL_cm),2) * 10,
            se.pre.fl = round(sd((Pre_FL_cm) * 10/sqrt(n)),2),
            mean.pre.wt = round(mean(Pre_Weight_g),2),
            se.pre.wt = round((sd(Pre_Weight_g) * 10/sqrt(n)),2)) 

SumTable_Post <- Cagetype_data %>%
  group_by(Site, Mesh) %>%
  drop_na(Post_FL_cm) %>%
  summarize(
            n = n(),
            mean.Post.fl = round(mean(Post_FL_cm),2) * 10,
            se.Post.fl = round(sd((Post_FL_cm) * 10/sqrt(n)),2),
            mean.Post.wt = round(mean(Post_Weight_g),2),
            se.Post.wt = round((sd(Post_Weight_g) * 10/sqrt(n)),2)) 


### Fork Length --------------------------------------------------------------------
## Calculate mean, sd, median for FL
Cage.FL.stat <- Cage %>%
  na.omit() %>%
  group_by(Site, Cage)%>%
  summarize(
    FL_pre_mean = mean(Pre_FL_cm),
    FL_pre_sd = sd(Pre_FL_cm),
    FL_post_mean = mean(Post_FL_cm),
    FL_post_sd = sd(Post_FL_cm),
    FL_pre_median = median(Pre_FL_cm),
    FL_post_median = median(Post_FL_cm),
    N = n())

# Write summary table
# write.csv(Cage.stat.FL, "CSV_Output/FL.csv")

# Size (FL) distributions - plot
ggplot(Cage_FL) +
  geom_jitter(aes(Mesh, FL, col = Site)) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Violin plots for FL 
  # Split up controls 
Cage_FL_DWSC <- Cage_FL%>%
  filter(Site == "DWSC")
Cage_FL_RV <- Cage_FL%>%
  filter(Site == "RV")
  # Plot
ggplot(Cage_FL) +
  geom_violin(aes(Pre_Post, FL, fill = Mesh), position = position_dodge(1)) +
  facet_wrap(~Site) +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.position = "bottom")


ggplot(Cage_FL_RV) +
  geom_violin(aes(Pre_Post, FL, fill = Mesh), position = position_dodge(1)) +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Pre-Post Boxplot By site
ggplot(Cage_FL, aes(x = Site, y = FL, fill = Pre_Post)) +
  geom_boxplot() +
  labs(y = "FL(cm)")+
  scale_fill_manual(values = c("#587081", "#45b5c5"))+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 14))

# Pre-Post Boxplot Size by Site and Cage #########
ggplot(Cage_FL, aes(x = Mesh, y = FL, fill = Pre_Post)) +
  geom_boxplot() +
  labs(y = "Fork Length (cm)")+
  scale_fill_manual(values = c("#587081", "#45b5c5"))+
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 14))

### Weight ------------------------------------------------------------------
## Weight Summary Table
# Calculate mean, median, sd for Weight
Cage.Wt.stat <- Cage %>%
  na.omit() %>%
  group_by(Site, Cage)%>%
  summarize(
    Weight_pre_mean = mean(Pre_Weight_g),
    Weight_pre_sd = sd(Pre_Weight_g),
    Weight_post_mean = mean(Post_Weight_g),
    Weight_post_sd = sd(Post_Weight_g),
    Weight_pre_median = median(Pre_Weight_g),
    Weight_post_median = median(Post_Weight_g),
    N = n())

# Write file
# write.csv(Cage.Wt.stat, "CSV_Output/Wt.csv")

# Weight distributions - plot
ggplot(Cage_Weight) +
  geom_jitter(aes(Cage, Weight, col = Site)) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Boxplot of Weight by site
  #remove controls
Cage_Weight_2 <- Cage_Weight %>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2")

  # plot
ggplot(Cage_Weight_2, aes(x = Site, y = Weight, fill = Pre_Post)) +
  geom_boxplot() +
  labs(y = "Weight (g)")+
  scale_fill_manual(values = c("#587081", "#45b5c5"))+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 14))

# Boxplot by site and cage
ggplot(Cage_Weight, aes(x = Mesh, y = Weight, fill = Pre_Post)) +
  geom_boxplot() +
  labs(y = "Weight (g)")+
  facet_wrap(~Site) +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 16))

# Delta Weight
Cage$Delta_Wt <- Cage$Post_Weight_g-Cage$Pre_Weight_g

ggplot(Cage, aes(x = Site, y = Delta_Wt, fill = Cage)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Delta Weight (g)")+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 14))

### Condition Factor --------------------------------------------------------------

Cage.CF <- Cagetype_data %>%
  na.omit() %>%
  filter(Cage!="FCCL1") %>%
  filter(Cage!="FCCL2") %>%
  group_by(Site, Mesh)%>%
  summarize(
    Delta_CF_Mean = mean(Delta_CF),
    Delta_CF_sd = sd(Delta_CF),
    Delta_CF_median = median(Delta_CF),
    N = n()
  )

Cage.box <- Cage %>%
  na.omit() %>%
  group_by(Site, Cage)%>%
  summarize(
    Delta_CF_Mean = mean(Delta_CF),
    Delta_CF_sd = sd(Delta_CF),
    Delta_CF_median = median(Delta_CF),
    N = n()
  )

# write.csv(Cage.CF, "Condition_Factors.csv")

# Pre-Post Boxplot Size by Site and Cage #########
ggplot(Cage_CF, aes(x = Mesh, y = CF, fill = Pre_Post)) +
  geom_boxplot() +
  labs(y = "Condition Factor")+
  scale_fill_manual(values = c("violetred1", "violetred4"))+
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 14))

# Pre-Condition Factor 
ggplot(Cage, aes(Site, Pre_CF, fill = Cage)) + geom_boxplot() +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

# Delta CF Mean by cage  
ggplot(Cage.CF, aes(Mesh, Delta_CF_Mean, fill = Site)) + 
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Mean Delta Condition Factor") +
  geom_errorbar(aes(ymin = Delta_CF_Mean - Delta_CF_sd, ymax = Delta_CF_Mean + Delta_CF_sd), 
                position = position_dodge(width = 0.9),
                width = 0.1) +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

# CF mean by site
ggplot(Cage.CF, aes(Mesh, Delta_CF_Mean,  fill = Mesh)) + 
  facet_wrap(~Site)+
  geom_bar(position = "dodge2", stat = "identity") +
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Mean Delta Condition Factor") +
  geom_errorbar(aes(ymin = Delta_CF_Mean - Delta_CF_sd, ymax = Delta_CF_Mean + Delta_CF_sd), width = 0.1, 
                position = position_dodge(0.9), color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.position = "none")

# CF boxplot by site
Cageonly <- Cagetype_data %>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2")

ggplot(Cageonly, aes(Mesh, Delta_CF,  fill = Mesh)) +
  facet_wrap(~Site) +
  labs(y = "Delta Condition Factor") +
  geom_boxplot()+
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 16),
        legend.position = "none")

ggplot(Cageonly, aes(Site, Delta_CF,  fill = Site)) +
  labs(y = "Delta Condition Factor") +
  geom_boxplot()+
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 16),
        legend.position = "none")

# no outlier
ggplot(Cage, aes(Site, Delta_CF,  fill = Cage)) +
labs(y = "Delta Condition Factor") +
  geom_boxplot(outlier.shape = NA)+
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

