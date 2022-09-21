# 12/19/19
# Catarina Pien
# Comparison of Mesh types 
# Anova for Delta Smelt Condition Factor 
# Chi Square for Delta Smelt Survival 

# Set up workspace and load data --------------------------
rm(list=ls(all=TRUE))

library(car)
library(FSA)
library(tidyverse)
library(ggthemes)
library(here)
library(dplyr)

Cagetype_data <- read.csv(here("smelt_2019_winterspring", "data_clean", "Cagetype_data.csv")) %>%
  mutate(Site = factor(Site, levels = c("RV", "DWSC")),
         Enclosure = ifelse(Mesh == "wrap", "A", ifelse(Mesh == "large", "B", ifelse(Mesh == "small", "C", ifelse(Mesh == "control", "control", NA)))))

unique(Cagetype_data$Enclosure)

# Summary Table Info ----
# Pre
Pre_info <- Cagetype_data %>%
  select(c(Cage:Pre_CF), Site:Mesh) %>%
  mutate(Treatment = "Pre") %>%
  rename(Weight = Pre_Weight_g,
         FL = Pre_FL_cm,
         CF = Pre_CF)
# Post
Post_info <- Cagetype_data %>%
  select(c(Cage, Post_FL_cm:Post_CF,Site:Mesh)) %>%
  mutate(Treatment = "Post") %>%
  rename(Weight = Post_Weight_g,
         FL = Post_FL_cm,
         CF = Post_CF)

Table_info <- rbind(Pre_info, Post_info)

NAs <- filter(Table_info, is.na(CF))

# Combined
Table_summary <- Table_info %>%
  filter(!is.na(CF)) %>%
  group_by(Site, Mesh, Treatment) %>%
  summarize(meanFL = mean(FL),
            meanWeight = mean(Weight),
            n = n())







################## Condition Factor ----

# Make complete cases from column 9
Delta_CF <- Cagetype_data[,9]
Cagetype_CF <- Cagetype_data[complete.cases(Delta_CF),]

#write.csv(Cagetype_CF, "CSV_Output/CageCFData_Clean_Jan2019Deploy.csv")


# Summarize mean, sd, median, n for condition factor
Cage.CF <- Cagetype_CF %>%
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

# Summarize mean, sd, median, n for condition factor
Cage.LW <- Cagetype_CF %>%
  na.omit() %>%
  filter(Cage!="FCCL1") %>%
  filter(Cage!="FCCL2") %>%
  group_by(Site, Mesh)%>%
  summarize(
    Pre_L = mean(Pre_FL_cm),
    Post_L = mean(Post_FL_cm),
    Pdiff_L = round((Post_L-Pre_L)/Pre_L*100,2),
    n = n(),
    Pre_W = mean(Pre_Weight_g),
    Post_W = mean(Post_Weight_g),
    Pdiff_W = round((Post_W-Pre_W)/Pre_W*100,2),
  )

# Calculate Average CF for FCCL
FCCL_CF <- Cagetype_CF %>%
  filter(Cage == "FCCL1" | Cage == "FCCL2") %>%
  group_by(Cage, Site) %>%
  summarize(mean.PreCF = mean(Pre_CF),
            mean.PostCF = mean(Post_CF)) %>%
  mutate(delta.CF = round(mean.PostCF - mean.PreCF,2))


#write.csv(Cage.CF, "Condition_Factors.csv")

# Plots -----

# * Overview of data (geom_point) ----
ggplot(Cagetype_CF, aes(Mesh, Delta_CF, col = Mesh)) + 
  facet_wrap(~Site) +
  geom_jitter(size = 3) +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

ggplot(Cagetype_CF, aes(Cage, Delta_CF, col = Cage)) + 
  facet_wrap(~Site) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2")+
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))



# * CF mean and sd by site ----
# CF mean by site
ggplot(Cage.CF, aes(Mesh, Delta_CF_Mean,  fill = Mesh)) + 
  facet_wrap(~Site)+
  geom_bar(width = 0.8, position = "dodge2", stat = "identity") +
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Delta Condition Factor") +
  geom_errorbar(aes(ymin = Delta_CF_Mean - Delta_CF_sd, ymax = Delta_CF_Mean + Delta_CF_sd), width = 0.1, 
                position = position_dodge(0.9), color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.position = "none")

# Mean point plot
ggplot(Cage.CF, aes(Mesh, Delta_CF_Mean,  col = Mesh)) + 
  facet_wrap(~Site)+
  geom_point(size = 8, shape = 18) +
  scale_colour_brewer(palette="Dark2")+
  labs(y = "Delta Condition Factor") +
  geom_errorbar(aes(ymin = Delta_CF_Mean - Delta_CF_sd, ymax = Delta_CF_Mean + Delta_CF_sd), width = 0.1, 
                position = position_dodge(0.9), color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.position = "none")

# * CF boxplot by site ----
Cageonly <- Cagetype_data %>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2") %>%
  mutate(Group = ifelse(Site == "RV" & Mesh == "wrap", "B", ifelse(Site == "DWSC", " ", "A")))

windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL \u0394 CF = 0.11","FCCL \u0394 CF = 0.02"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(0.16, 0.07))
ann_text$Site <- factor(ann_text$Site, levels = c("RV", "DWSC"))


# Boxplot by enclosure type 
ggplot(data=Cageonly) +
  geom_boxplot(aes(x=Enclosure, y=Delta_CF,  fill = Enclosure)) +
  facet_wrap(~Site) +
  labs(y = "Delta Condition Factor") +
  geom_text(aes(label = Group, x = Enclosure, y = -0.32), family = "Times", size = 6) +
  geom_hline(data = FCCL_CF, aes(yintercept=delta.CF), size = 1) +
  geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "\u0394 Condition Factor", x = "Enclosure Type") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"))

Cageonly$Site <- factor(Cageonly$Site, levels = c("RV", "DWSC"))
levels(Cageonly$Site)

# * Plot in manuscript below ------------------------
# No Dunn Test results here
(Boxplot <- ggplot(data=Cageonly) +
  geom_boxplot(aes(x=Enclosure, y=Delta_CF,  fill = Enclosure)) +
  facet_wrap(~Site) +
  labs(y = "Delta Condition Factor") +
 # geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  geom_hline(data = FCCL_CF, aes(yintercept=delta.CF), size = 1) +
  geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "\u0394 Condition Factor", x = "Enclosure Type") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.spacing = unit(2, "lines")))

# * Write plot --------------
tiff(filename = "smelt_2019_winterspring/figures/Figure_deltacfboxplot.tiff", pointsize = 12, res = 300, units = "in", width = 8, height = 5)
Boxplot
dev.off()



# * CF boxplot by cage ----
ggplot(data=Cageonly, aes(x=Cage, y=Delta_CF,  fill = Mesh)) +
  facet_wrap(~Site) +
  labs(y = "Delta Condition Factor") +
  geom_boxplot()+
  geom_hline(data = FCCL_CF, aes(yintercept=delta.CF)) +
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Delta Condition Factor") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 16),
        legend.position = "none")

## RV -----------------------------

# * Run ANOVA ----

Cage_only_RVR <- Cagetype_CF %>%
  filter(Site == "RV")%>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2")

rv.cf.aov <- aov(Delta_CF~Mesh, data = Cage_only_RVR)
summary(rv.cf.aov)

M.RVR.1 <- lm(Delta_CF~Mesh, data = Cage_only_RVR)
Anova(M.RVR.1, type = "2")

# * * Check assumptions -----------------------------------
# Homogeneity of variances
par(mfrow = c(1,2))
plot(rv.cf.aov, 1)
leveneTest(Delta_CF ~ Mesh, data = Cage_only_RVR) # p<0.05 means not homogeneous; however, not too deviant in this case

# Normality
plot(rv.cf.aov, 2)

rv_cf_residuals <- residuals(object = rv.cf.aov)
shapiro.test(x = rv_cf_residuals ) # p<0.05 means not normal
shapiro.test(log(Cage_only_RVR$Delta_CF+1))


# * Try a mixed effects model ----
library(lme4)
library(lmerTest)
library(car)

# * * Run model ----
hist(Cage_only_RVR$Delta_CF)
M.RVR = lmer((Delta_CF)^1/3 ~ Mesh + (1|Cage), REML = TRUE, data = Cage_only_RVR, na.action = na.omit)

M.RVR2 = lmer(log(Delta_CF+1) ~ Mesh + (1|Cage), REML = TRUE, data = Cage_only_RVR, na.action = na.omit)
summary(M.RVR2)

M.RVR4 = lmer(Delta_CF ~ Mesh + (1|Cage), REML = TRUE, data = Cage_only_RVR, na.action = na.omit)

M.RVR3 = lmer((Delta_CF)^1/5 ~ Mesh + (1|Cage), REML = TRUE, data = Cage_only_RVR, na.action = na.omit)

M.RVR5 = lmerTest::lmer(Delta_CF ~ Mesh + (1|Cage), REML = TRUE, data = Cage_only_RVR, na.action = na.omit)
summary(M.RVR5)


summary(M.RVR4)
Anova(M.RVR4, type = "2")

AIC(M.RVR, M.RVR2, M.RVR3, M.RVR4)

library(emmeans)
m_means <- emmeans(M.RVR2, pairwise~Mesh, adjust="sidak")
m_means

# Compare cages with Mann Whitney U - if no effect, Kruskal Wallis
MeshSm <- filter(Cage_only_RVR, Mesh == "small")
MeshWrap <- filter(Cage_only_RVR, Mesh == "wrap")
MeshL <- filter(Cage_only_RVR, Mesh == "large")

wilcox.test(Delta_CF~Cage, data = MeshSm)
wilcox.test(Delta_CF~Cage, data = MeshWrap)
wilcox.test(Delta_CF~Cage, data = MeshL)

# Unfortunately, they are all significantly different

# Otherwise do ANOVA and explain the error associated, given non-normality

M1.resid2 <- resid(M.RVR2, type = "pearson")
plot(M.RVR2)
rv_cf_residuals3 <- residuals(object = M.RVR2)
shapiro.test(x = rv_cf_residuals3 ) # p<0.05 means not normal, W to 0.96
hist(M1.resid2)
par(mfrow = c(1,1))
qqnorm(M1.resid2)
qqline(M1.resid2)

M1.resid3 <- resid(M.RVR3, type = "pearson")
plot(M.RVR3)
rv_cf_residuals4 <- residuals(object = M.RVR3)
shapiro.test(x = rv_cf_residuals4 ) # p<0.05 means not normal
hist(M1.resid3)
par(mfrow = c(1,1))
qqnorm(M1.resid2)
qqline(M1.resid2)




# * * Check assumptions ----
M1.resid <- resid(M.RVR, type = "pearson")
plot(M.RVR)
plot(M.RVR, form = resid(., type = "pearson") ~ fitted(.) | Mesh, abline = 0, 
     cex = .5, pch = 20, col = "black")

# normality
par(mfrow = c(1,1))
diagQQ <- qqnorm(M1.resid)
qqline(M1.resid)
hist(M1.resid)
rv_cf_residuals2 <- residuals(object = M.RVR)
shapiro.test(x = rv_cf_residuals2 ) # p<0.05 means not normal

# observed vs fitted
# There is no funnel
plot(M.RVR, Delta_CF ~ fitted(.), id = 0.05, adj = -0.3, 
      cex = .8, pch = 20, col = "blue")
# leverage
ggplot(data.frame(lev=hatvalues(M.RVR),pearson=residuals(M.RVR,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

# variance of residuals by mesh
ggplot(Cage_only_RVR, aes(x = Mesh, y = M1.resid)) + geom_boxplot()
leveneTest(Delta_CF ~ Mesh, data = Cage_only_RVR)

# variance of data by mesh
ggplot(Cage_only_RVR, aes(x = Mesh, y = Delta_CF)) + geom_boxplot()

# * * Plot results ----
library(MuMIn) #extract R2
library(sjPlot)

r.squaredGLMM(M.RVR4)
plot_model(M.RVR4, type = "pred", terms = c("Mesh"))
RVRmod <- tab_model(M.RVR4, digits = 3)
write.tab

# * Kruskal-Wallis test--------------------
kruskal.test(Delta_CF~Mesh, data = Cage_only_RVR) # still significant

# * * Post-hoc Dunn test --------------
# Dunn test can be used if sample size are heterogeneous
RV.dunn <- dunnTest(Delta_CF~Mesh,
                    data = Cage_only_RVR,
                    method = "bonferroni") # bonferroni corrects for number of comparisons
RV.dunn
RV.dunn = RV.dunn$res

library(rcompanion)

cldList(comparison = RV.dunn$Comparison,
        p.value    = RV.dunn$P.adj,
        threshold  = 0.05)

## DWSC ---------------------------------------------------------------------------

# * Run ANOVA -------------------------------------

Cage_only_DWSC <- Cagetype_CF %>%
  filter(Site == "DWSC") %>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2")

Cage_only_DWSC$Mesh <- as.factor(Cage_only_DWSC$Mesh) 

dwsc.cf.aov <- aov(Delta_CF~Mesh, data = Cage_only_DWSC)
summary(dwsc.cf.aov)

DWSC1 <- lm(Delta_CF~Mesh, data = Cage_only_DWSC)
Anova(DWSC1, type = "2")

TukeyHSD(dwsc.cf.aov)

# * * Check assumptions -----------------------------------
# Homogeneity of variances
plot(dwsc.cf.aov, 1)
leveneTest(Delta_CF ~ Mesh, data = Cage_only_DWSC) # p<0.05 means not homogeneous

# Normality
plot(dwsc.cf.aov, 2)

dwsc_cf_residuals <- residuals(object = dwsc.cf.aov)
shapiro.test(x = dwsc_cf_residuals ) # p<0.05 means not normal

# * Mixed Effects Model ----

# * * Run model ----
hist(Cage_only_DWSC$Delta_CF)
M.DWSC = lmer(Delta_CF ~ Mesh + (1|Cage), data = Cage_only_DWSC, na.action = na.omit)
summary(M.DWSC)
Anova(M.DWSC, type = "2")

# * * Check assumptions ----
M2.resid <- resid(M.DWSC, type = "pearson")

plot(M.DWSC)
plot(M.DWSC, form = resid(., type = "pearson") ~ fitted(.) | Mesh, abline = 0, 
     cex = .5, pch = 20, col = "black")

# normality
par(mfrow = c(1,1))
qqnorm(M2.resid)
qqline(M2.resid)
hist(M2.resid)

# observed vs fitted
# There is no funnel
plot(M.DWSC, Delta_CF ~ fitted(.), id = 0.05, adj = -0.3, 
     cex = .8, pch = 20, col = "blue")
# leverage
ggplot(data.frame(lev=hatvalues(M.DWSC),pearson=residuals(M.DWSC,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

# variance by mesh
ggplot(Cage_only_DWSC, aes(x = Mesh, y = M2.resid)) + geom_boxplot()
leveneTest(Delta_CF ~ Mesh, data = Cage_only_DWSC)

# variance of data by mesh
ggplot(Cage_only_DWSC, aes(x = Mesh, y = Delta_CF)) + geom_boxplot()

# * * Plot results ----
library(MuMIn) #extract R2
library(sjPlot)

r.squaredGLMM(M.DWSC)
plot_model(M.DWSC, type = "pred", terms = c("Mesh"))
tab_model(M.DWSC)

# Compare cages with Mann Whitney U - if no effect, Kruskal Wallis
MeshSmD <- filter(Cage_only_DWSC, Mesh == "small")
MeshWrapD <- filter(Cage_only_DWSC, Mesh == "wrap")
MeshLD <- filter(Cage_only_DWSC, Mesh == "large")

wilcox.test(Delta_CF~Cage, data = MeshSmD) #ns
wilcox.test(Delta_CF~Cage, data = MeshWrapD) #barely sig
wilcox.test(Delta_CF~Cage, data = MeshLD) #ns


# * Kruskal-Wallis test. Does not meet assumptions: do non-parametric ---------------
kruskal.test(Delta_CF~Mesh, data = Cage_only_DWSC) 


########################### Survival ##############################################---------------------------------------------------------------------------------

# Reorganize data
# Make complete cases from column 7 (post_weight)
post_fish <- Cagetype_data[,7]
Cagetype_s <- Cagetype_data[complete.cases(post_fish),]

# ----------------Rio Vista-----------------------------------------
# Calculate sample sizes for alive and dead
surv_RV <- Cagetype_s %>%
  filter(Site == "RV") %>%
  group_by(Site, Cage, Mesh)%>%
  summarize(
    Alive = n(),
    Dead = 64-n()
  )

# Create data frame of each individual fish - survived or did not survive
Cage_survival_RV <- as.data.frame(matrix(0, ncol = 2, nrow = 384))
colnames(Cage_survival_RV) <- c("Cage", "Survived")

# Add dead fish from data sheet
  # Add RN82 from cage A that was decomposed
  # RG49 from cage A already included in the dead count (no Weight data)
  # Add RJ08 from cage E that was dead prior, stomach torn open

Cage_survival_RV$Cage <- c(rep("A", 64), rep("B", 64), rep("C", 64), rep("D", 64), rep("E", 64), rep("F", 64))
Cage_survival_RV$Survived <- c(rep("1", 60), rep("0", 4), 
                               rep("1", 64), rep("0", 0),
                               rep("1", 64), rep("0", 0),
                               rep("1", 64), rep("0", 0),
                               rep("1", 62), rep("0", 2),
                               rep("1", 56), rep("0", 8))

Cagetype_survival_RV <- left_join(Cage_survival_RV, surv_RV, by = "Cage")

Cagetype_survival_RV <- Cagetype_survival_RV %>%
  mutate(Cage = factor(Cage),
         Survived = factor(Survived),
         Site = factor(Site),
         Mesh = factor(Mesh))

# * Chi square test  ------------------------------
# Does surival differ significantly between meshes?
RV_survival <- table(Cagetype_survival_RV$Mesh, Cagetype_survival_RV$Survived)

(RV_survival_chi <- chisq.test(RV_survival)) # Significant
# Fisher test for smaller sample sizes (some have n<5)

fisher.test(RV_survival, alternative = "two.sided") # Significant

# 2 x 2 
###### Large-small
RV_large_small <- Cagetype_survival_RV %>%
  filter(Mesh != "wrap")
RV_large_small_t <- table(RV_large_small$Mesh, RV_large_small$Survived)
fisher.test(RV_large_small_t, alternative = "one.sided") # Significant

fisher.test(RV_large_small_t)$p.value/3 # Bonferroni correction: Significant

p.adjust(fisher.test(RV_large_small_t)$p.value, method = "bonferroni", 
         n = 3) # Not Significant

###### Large-wrap
RV_large_wrap <- Cagetype_survival_RV %>%
  filter(Mesh != "small")
RV_large_wrap_t <- table(RV_large_wrap$Mesh, RV_large_wrap$Survived)
fisher.test(RV_large_wrap_t, alternative = "one.sided") # Not Significant

###### Small-wrap
RV_small_wrap <- Cagetype_survival_RV %>%
  filter(Mesh != "large")
RV_small_wrap_t <- table(RV_small_wrap$Mesh, RV_small_wrap$Survived)
fisher.test(RV_small_wrap_t, alternative = "one.sided") # Significant

p.adjust(fisher.test(RV_small_wrap_t)$p.value, method = "bonferroni", 
         n = 3) # Not Significant

# Logistic mixed model ----

library(lme4)
library(lmerTest)

mRV_surv <- glmer(Survived ~ Mesh + (1|Cage), data = Cagetype_survival_RV, family = binomial(link = "logit"))

summary(mRV_surv)
se <- sqrt(diag(vcov(mRV_surv)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(mRV_surv), LL = fixef(mRV_surv) - 1.96 * se, UL = fixef(mRV_surv) + 1.96 *
                se))
# * * Check assumptions ----
Ms1.resid <- resid(mRV_surv, type = "pearson")
plot(mRV_surv)
plot(mRV_surv, form = resid(., type = "pearson") ~ fitted(.) | Mesh, abline = 0, 
     cex = .5, pch = 20, col = "black")

# normality
par(mfrow = c(1,1))
qqnorm(Ms1.resid)
qqline(Ms1.resid)
hist(Ms1.resid)

# observed vs fitted
# There is no funnel
plot(mRV_surv, Survived ~ fitted(.), id = 0.05, adj = -0.3, 
     cex = .8, pch = 20, col = "blue")
# leverage
ggplot(data.frame(lev=hatvalues(mRV_surv),pearson=residuals(mRV_surv,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

# variance by mesh
ggplot(Cagetype_survival_RV, aes(x = Mesh, y = Ms1.resid)) + geom_boxplot()

leveneTest(Survived ~ Mesh, data = Cage_only_DWSC)

# * * Plot results ----
library(MuMIn) #extract R2
library(sjPlot)
library(sjstats) # for intraclass-correlation coefficient
library(caret) # model comparison & utility functions

r.squaredGLMM(mRV_surv)
plot_model(mRV_surv, type = "pred", terms = c("Mesh"))
tab_model(mRV_surv)

library(ggeffects)
ggpredict(mRV_surv, "Survived")


#--------------DWSC----------------------------------------------------
surv_DWSC <- Cagetype_s %>%
  filter(Site == "DWSC") %>%
  group_by(Site, Cage, Mesh)%>%
  summarize(
    Alive = n(),
    Dead = 60-n()
  )
# Add dead fish
  # Add NTF4 to dead in Cage F - "decomposed"
  # Add RB61 to dead in Cage F - "partly decomposed"

Cage_survival_DWSC <- as.data.frame(matrix(0, ncol = 2, nrow = 360))
colnames(Cage_survival_DWSC) <- c("Cage", "Survived")

Cage_survival_DWSC$Cage <- c(rep("A", 60), rep("B", 60), rep("C", 60), rep("D", 60), rep("E", 60), rep("F", 60))
Cage_survival_DWSC$Survived <- c(rep("1", 59), rep("0", 1), 
                                 rep("1", 60), rep("0", 0),
                                 rep("1", 58), rep("0", 2),
                                 rep("1", 60), rep("0", 0),
                                 rep("1", 60), rep("0", 0),
                                 rep("1", 58), rep("0", 2))

Cagetype_survival_DWSC <- left_join(Cage_survival_DWSC, surv_DWSC, by = "Cage")

Cagetype_survival_DWSC <- Cagetype_survival_DWSC %>%
  mutate(Cage = factor(Cage),
         Survived = factor(Survived),
         Site = factor(Site),
         Mesh = factor(Mesh))

# * Chi square test  ----------------------------
# Does surival differ significantly between meshes?
DWSC_survival <- table(Cagetype_survival_DWSC$Mesh, Cagetype_survival_DWSC$Survived)

(DWSC_survival_chi <- chisq.test(DWSC_survival)) # Not significant: No!
# Fisher test for smaller sample sizes (some have n<5)
fisher.test(DWSC_survival) # Not significant: No!

# Logistic mixed model ----

library(lme4)
library(lmerTest)

#logit transform survival data
#lmer with random effect cage
#anova

mDWSC_surv <- glmer(Survived ~ Mesh + (1|Cage), data = Cagetype_survival_DWSC, family = binomial(link = "logit"))
# is singular

summary(mDWSC_surv)

se <- sqrt(diag(vcov(mDWSC_surv)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(mDWSC_surv), LL = fixef(mDWSC_surv) - 1.96 * se, UL = fixef(mDWSC_surv) + 1.96 *
                se))
plot(mDWSC_surv)

# * * Check assumptions ----
Ms2.resid <- resid(mDWSC_surv, type = "pearson")
plot(mDWSC_surv)
plot(mDWSC_surv, form = resid(., type = "pearson") ~ fitted(.) | Mesh, abline = 0, 
     cex = .5, pch = 20, col = "black")

# normality
par(mfrow = c(1,1))
qqnorm(Ms2.resid)
qqline(Ms2.resid)
hist(Ms2.resid)

# leverage
ggplot(data.frame(lev=hatvalues(mDWSC_surv),pearson=residuals(mDWSC_surv,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

# check variance
ggplot(Cagetype_survival_DWSC, aes(x = Mesh, y = Ms2.resid)) + geom_boxplot()


# * * Plot results ----
library(MuMIn) #extract R2
library(sjPlot)
library(sjstats) # for intraclass-correlation coefficient
library(caret) # model comparison & utility functions

r.squaredGLMM(mDWSC_surv)
plot_model(mDWSC_surv, type = "pred", terms = c("Mesh"))
tab_model(mDWSC_surv)



# Survival Plot ---------------------------------------
# * By Mesh ----


RV_survival_sum <- Cagetype_survival_RV %>%
  group_by(Mesh) %>%
  summarize(Survive = sum(Survived == 1),
            Died = -1 * sum(Survived == 0),
            n = n()) %>%
  pivot_longer(cols = c(Survive, Died), names_to = "Survival", values_to = "Count")

DWSC_survival_sum <- Cagetype_survival_DWSC %>%
  group_by(Mesh) %>%
  summarize(Survive = sum(Survived == 1),
            Died = -1 * sum(Survived == 0),
            n = n()) %>%
  pivot_longer(cols = c(Survive, Died), names_to = "Survival", values_to = "Count")

DWSC_survival_sum$Site <- "DWSC"
RV_survival_sum$Site <- "RV"

Survival_sum <- rbind(DWSC_survival_sum, RV_survival_sum)
Survival_sum <- Survival_sum %>%
  mutate(prop = ifelse(Survival == "Survive", round(Count/n * 100,0), round((-1 * Count/n * 100),0)))

# Survival plot 
ggplot(Survival_sum, aes(Mesh, Count, fill = Survival)) +
  facet_wrap(~Site) +
  geom_col(width = 0.55) + 
  coord_cartesian(y = c(-30,130)) +
  scale_y_continuous(breaks = seq(0, 125, by = 25)) +
  geom_text(aes(label = paste0(prop, "%")), vjust = 1.5, size = 5)+
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Set1")+
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 17),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        legend.position = "none")



# * By Cage ----
RV_survival_sum2 <- Cagetype_survival_RV %>%
  group_by(Mesh, Cage) %>%
  summarize(Survive = sum(Survived == 1),
            Died = -1 * sum(Survived == 0),
            n = n()) %>%
  pivot_longer(cols = c(Survive, Died), names_to = "Survival", values_to = "Count")

DWSC_survival_sum2 <- Cagetype_survival_DWSC %>%
  group_by(Mesh, Cage) %>%
  summarize(Survive = sum(Survived == 1),
            Died = -1 * sum(Survived == 0),
            n = n()) %>%
  pivot_longer(cols = c(Survive, Died), names_to = "Survival", values_to = "Count")

DWSC_survival_sum2$Site <- "DWSC"
RV_survival_sum2$Site <- "RV"

Survival_sum2 <- rbind(DWSC_survival_sum2, RV_survival_sum2)
Survival_sum2 <- Survival_sum2 %>%
  mutate(prop = ifelse(Survival == "Survive", round(Count/n * 100,0), round((-1 * Count/n * 100),0)))

ggplot(Survival_sum2, aes(Cage, Count, fill = Survival)) +
  facet_wrap(~Site) +
  geom_col(width = 0.7, aes(colour = Mesh), size = 3) + 
  coord_cartesian(y = c(-30,130)) +
  scale_y_continuous(breaks = seq(0, 125, by = 25)) +
  scale_color_brewer(palette="Dark2")+
  geom_text(aes(label = paste0(prop, "%")), vjust = 1.5, size = 5)+
  geom_hline(yintercept=0)+
  scale_fill_brewer(palette="Set1")+
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 17),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        legend.position = "none")

### Mean survival

Survival_sum3 <- Survival_sum2 %>%
  mutate(prop = ifelse(Survival == "Survive", round(Count/n * 100,0), (-1 * Count/n * 100)))

propSurvival <- filter(Survival_sum3, Survival == "Survive") %>%
  ungroup() %>%
  #group_by(Site) %>%
  summarize(meanSurvival = mean(prop),
            sdSurvival = sd(prop))
