# 5/10/21
# Haley Hudson 
# Creating Figures/Tables for Weight and Length
#for additional information along with condition factor

# Set up workspace and load data ----------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(car)
library(FSA)
library(tidyverse)
library(ggthemes)

Cagetype_data <- read.csv(here("smelt_2019_winterspring", "data_clean", "Cagetype_data.csv")) %>%
  mutate(Site = factor(Site, levels = c("RV", "DWSC")),
         Enclosure = ifelse(Mesh == "wrap", "A", ifelse(Mesh == "large", "B", ifelse(Mesh == "small", "C", ifelse(Mesh == "control", "control", NA)))))

# ----------------------------------------------------------------------------------
################## Condition Factor ################################################
# -----------------------------------------------------------------------------------

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

# Calculate Average CF for FCCL
FCCL_CF <- Cagetype_CF %>%
  filter(Cage == "FCCL1" | Cage == "FCCL2") %>%
  group_by(Cage, Site) %>%
  summarize(mean.PreCF = mean(Pre_CF),
            mean.PostCF = mean(Post_CF)) %>%
  mutate(delta.CF = round(mean.PostCF - mean.PreCF,2))
#write.csv(Cage.CF, "Condition_Factors.csv")

###############################################################################
#                      Length and Weight

# Summarize mean, sd, median, n for Pre and Post Length and Width
Cagedata <- Cagetype_CF %>%
  na.omit() %>%
  filter(Cage!="FCCL1") %>%
  filter(Cage!="FCCL2") %>%
  group_by(Site, Mesh)%>%
  summarize(
    PreL_Mean = mean(Pre_FL_cm),
    PreL_sd = sd(Pre_FL_cm),
    PreL_median = median(Pre_FL_cm),
    PoL_Mean = mean(Post_FL_cm),
    PoL_sd = sd(Post_FL_cm),
    PoL_median = median(Post_FL_cm), 
    PreW_Mean = mean(Pre_Weight_g),
    PreW_sd = sd(Pre_Weight_g),
    PreW_median = median(Pre_Weight_g),
    PoW_Mean = mean(Post_Weight_g),
    PoW_sd = sd(Post_Weight_g),
    PoW_median = median(Post_Weight_g),  N = n()) %>%
    mutate(delta.Length = round(PoL_Mean - PreL_Mean,2)) %>%
  mutate(delta.weight = round(PoW_Mean - PreW_Mean,2)) %>%
  ungroup()

# Calculate Average Pre-length for FCCL Fish and sd, median, n for Length and weight 
FCCL <- Cagetype_CF %>%
  filter(Cage == "FCCL1" | Cage == "FCCL2") %>%
  group_by(Cage, Site) %>%
  summarize( PreL_Mean = mean(Pre_FL_cm),
             PreL_sd = sd(Pre_FL_cm),
             PreL_median = median(Pre_FL_cm),
             PoL_Mean = mean(Post_FL_cm),
             PoL_sd = sd(Post_FL_cm),
             PoL_median = median(Post_FL_cm), 
             PreW_Mean = mean(Pre_Weight_g),
             PreW_sd = sd(Pre_Weight_g),
             PreW_median = median(Pre_Weight_g),
             PoW_Mean = mean(Post_Weight_g),
             PoW_sd = sd(Post_Weight_g),
             PoW_median = median(Post_Weight_g),  N = n()) %>%
  mutate(delta.Length = round(PoL_Mean - PreL_Mean,2)) %>%
  mutate(delta.weight = round(PoW_Mean - PreW_Mean,2))

###############################################################################
#------------Tables-------------------------------------------------------

#Rename column
#FCCL<- FCCL %>% 
#  rename(
#    Mesh = Cage)

#Combine datasets 
#AllData<- rbind(Cagedata, FCCL)

#Create Table 
#DWSC_survival <- table(Cagetype_survival_DWSC$Mesh, Cagetype_survival_DWSC$Survived)


## Plots ------------------------------------------------------------------------


### Length barplot by site #####################
Cageonly <- Cagetype_data %>%
  filter(Cage != "FCCL1") %>%
  filter(Cage != "FCCL2") %>%
  mutate(Group = ifelse(Site == "RV" & Mesh == "wrap", "B", ifelse(Site == "DWSC", " ", "A")))  %>%
  mutate(delta.L = round(Post_FL_cm - Pre_FL_cm,2))%>%
  mutate(delta.W = round(Post_Weight_g - Pre_Weight_g,2))

CagePrePost <- gather(Cagetype_data, PrePost,Length1,Pre_FL_cm, Post_FL_cm)

windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL Delta Length = 1.50","FCCL Delta Length = 0.45"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(1.35, 0.65))

Lengthplot <- ggplot(data=CagePrePost,aes(x=PrePost, y=Length1)) +
  geom_bar(stat= "identity", aes(fill= Mesh), position="dodge") +
  facet_wrap(~Site) +
  labs(y = "Mean Length (cm)") + 
  #geom_errorbar(aes(x=PrePost, ymin=Length1-, ymax=Length1+sd))+
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  #geom_hline(data = FCCL, aes(yintercept=delta.Length), size = 1) +
  #geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Delta Length (cm)") +
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
Lengthplot

#######Plot with summerized data 
sumerized <- gather(Cagedata, PrePost ,Length1,PreL_Mean, PoL_Mean)

Lengthplot <- ggplot(data=sumerized,aes(x=Mesh, y=Length1)) +
  geom_bar(stat= "identity", aes(fill= PrePost), position="dodge") +
  facet_wrap(~Site) +
  labs(y = "Mean Length (cm)") + 
  #geom_errorbar(aes(x=Mesh, ymin=Length1-, ymax=Length1+sd))+
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  #geom_hline(data = FCCL, aes(yintercept=delta.Length), size = 1) +
  #geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Mean Length (cm)") +
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
Lengthplot
#################box length plot pre and post deployment
windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL Delta Weight = 1.57","FCCL Delta Weight = 0.46"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(1.44, 0.61))

CagePrePost$PrePost <- factor(CagePrePost$PrePost,
                       levels = c('Pre_FL_cm','Post_FL_cm'),ordered = TRUE)


################# MANUSCRIPT:: Length box plot by site ---------

MeanLenghtplot<- ggplot(data=CagePrePost) +
  geom_boxplot(outlier.size=-1,aes(x=Enclosure, y=Length1,  fill = PrePost)) +
  facet_wrap(~Site) +
  labs(y = "Length (cm)") +
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  #geom_hline(data = FCCL, aes(yintercept=delta.weight), size = 1) +
  #geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)++
  scale_fill_manual(values=c("#E7298A", "#66A61E"), name = "", labels = c("Pre-Deployment", "Post-Deployment")) +
  labs(y = "Length (cm)", x = "Enclosure Type") + scale_x_discrete(labels=c("control" = "FCCL")) +
  theme_minimal() + theme(axis.title.x = element_blank()) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  #scale_fill_discrete(name = "", labels = c("Pre-Deployment", "Post-Deployment")) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        panel.spacing = unit(2, "lines"))
MeanLenghtplot




################# MANUSCRIPT:: Weight box plot by site ---------
CagePrePostW <- gather(Cagetype_data, PrePostW,Weight1,Pre_Weight_g, Post_Weight_g)

windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL Delta Weight = 1.57","FCCL Delta Weight = 0.46"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(1.44, 0.61))

CagePrePostW$PrePostW <- factor(CagePrePostW$PrePostW,
                              levels = c('Pre_Weight_g','Post_Weight_g'),ordered = TRUE)

MeanWeightplot<- ggplot(data=CagePrePostW) +
  geom_boxplot(outlier.size=-1,aes(x=Enclosure, y=Weight1,  fill = PrePostW)) +
  facet_wrap(~Site) +
  labs(y = "Mean Weight (g)") +
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  #geom_hline(data = FCCL, aes(yintercept=delta.weight), size = 1) +
  #geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_manual(values=c("#E7298A", "#66A61E"), name = "Deployment", labels = c("Pre-Deployment", "Post-Deployment"))+
  #palette="Dark2" 
   labs(y = "Weight (g)", x = "Enclosure Type") + scale_x_discrete(labels=c("control" = "FCCL")) +
  theme_minimal() + theme(strip.text.x = element_blank()) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  #scale_fill_discrete(name = "Deployment", labels = c("Pre-Deployment", "Post-Deployment")) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        panel.spacing = unit(2, "lines"))
MeanWeightplot

########### violin plot ---------------------------------------------
windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL Delta Weight = 1.57","FCCL Delta Weight = 0.46"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(1.44, 0.61))

CagePrePost$PrePost <- factor(CagePrePost$PrePost,
                              levels = c('Pre_FL_cm','Post_FL_cm'),ordered = TRUE)

MeanLenghtvio<- ggplot(data=CagePrePost) +
  geom_violin(aes(x=Mesh, y=Length1,  fill = PrePost)) +
  facet_wrap(~Site) +
  labs(y = "Mean Length (cm)") +
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  #geom_hline(data = FCCL, aes(yintercept=delta.weight), size = 1) +
  #geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Mean Length (cm)") +
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
        #legend.position = "none",
        panel.spacing = unit(2, "lines"))
MeanLenghtvio


# Delta weight boxplot
windowsFonts(Times = windowsFont("Times New Roman"))

# Specify FCCL Delta CF label position
ann_text <- data.frame(
  label = c("FCCL Delta Weight = 1.57","FCCL Delta Weight = 0.46"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(1.44, 0.61))

Weightplot<- ggplot(data=Cageonly) +
  geom_boxplot(aes(x=Enclosure, y=delta.W,  fill = Enclosure)) +
  facet_wrap(~Site) +
  labs(y = "Delta Weight (g)") +
  #geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  geom_hline(data = FCCL, aes(yintercept=delta.weight), size = 1) +
  geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Delta Weight (g)") +
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
Weightplot

################ MANUSCRIPT PLOT ######################################
################Condition Factor -------------------------------------------
ann_text <- data.frame(
  label = c("FCCL Delta CF = 0.11","FCCL Delta CF = 0.02"),
  Site = c("RV", "DWSC"),
  x = c(2,2),
  y = c(0.16, 0.07))

ann_text$Site <- factor(ann_text$Site, levels = c("RV", "DWSC"))

CFplot<- ggplot(data=Cageonly) +
  geom_boxplot(aes(x=Enclosure, y=Delta_CF,  fill = Enclosure)) +
  facet_wrap(~Site) +
  labs(y = "Delta Condition Factor") +
  geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label), family = "Times", size = 5.5)+#geom_text(aes(label = Group, x = Mesh, y = -0.32), family = "Times", size = 6) +
  geom_hline(data = FCCL_CF, aes(yintercept=delta.CF), size = 1) +
  
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
CFplot


tiff(filename=file.path("smelt_2019_winterspring/figuresConditionFactorPlot.tiff"), units="in",type="cairo", bg="white", height=4, 
     width=7, res=300, pointsize=12,compression="lzw")
CFplot
dev.off()

###################################################################















################ MANUSCRIPT:: Combined Plot##################################
######-----------------------------Stack Plots------------------------------
library(ggpubr)
library(gridExtra)
library(grid)
#https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  # return gtable invisibly
  invisible(combined)
}

#--
tiff(filename=file.path("smelt_2019_winterspring/figures/WeightLengthPlot.tiff"), units="in",type="cairo", bg="white", height=4, 
     width=7, res=300, pointsize=12,compression="lzw")
combinedplot<-grid_arrange_shared_legend(MeanLenghtplot, MeanWeightplot, ncol = 1, nrow = 2)
dev.off()

#ggsave("Length_Weight.png", plot =combinedplot, dpi=300, height=7, width=10, units="in")
############################################################################
