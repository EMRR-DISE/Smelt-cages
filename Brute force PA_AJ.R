
# Load necessary packages
library(simr)
library(tidyverse)
library(lme4)
library(boot)
require(dplyr)

# From Brian's GLMM, we have a base detection probability of 0.05 for hard release at day 0
  # logit(0.05) = -2.95
  # and 0.04 for soft release (inv.logit(-2.95-0.2)~0.04), 
  # with declining detection probability with increasing days post-release (-0.07)
    # So at 30 days post-release, detection probability is down to 0.006

# Simulate hard release data, 20 trawls, 0.05 detection probability, no time effect
sim_data_hard <- rbinom(20, 1, 0.05) # add variability around 0.05

# Simulate soft release, 20 trawls, 0.06 detection probability, no time effect
sim_data_soft <- rbinom(20, 1, 0.06) # add variability around 0.06

df <- data.frame(hard=sim_data_hard, soft=sim_data_soft) %>%
  pivot_longer(cols=1:2, names_to="type")

test<- glm(value ~ type, data = df, family = "binomial")
summary(test)
ifelse(summary(test)$coef[8]<0.05 & summary(test)$coef[2]>0,1,0)
  # =1 if you successfully detect the right effect at p<0.05, otherwise =0

# Loop with no time effect

sims=1000
results <- numeric(sims)
trawls <- 800 # sampling effort
base_detect <- 0.05 # n=6000 released fish
soft_effect <- 1.2 # multiplicative difference between soft and hard release (>1 = better survival for soft)
for(i in 1:sims){
  sim_data_hard <- rbinom(trawls, 1, base_detect)
  sim_data_soft <- rbinom(trawls, 1, base_detect*1.2)
  df <- data.frame(hard=sim_data_hard, soft=sim_data_soft) %>%
    pivot_longer(cols=1:2, names_to="type")
  test<- glm(value ~ type, data = df, family = "binomial")
  results[i] <- ifelse(summary(test)$coef[8]<0.05 & summary(test)$coef[2]>0,1,0)
}

length(which(results==1))/length(results) # this value, *100%, provides the % power

# Power to detect 20% differences in survival, with detection probability ~0.05 
  # (equating roughly >=6000 released fish) and 800 trawls, is 12.5%
# This doesn't incorporate any site or time effects, but they could be added