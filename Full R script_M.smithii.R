# R script for M. smithii paper
library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)

# for pairwise comparison in linear mixed model
install.packages("emmeans")
library(emmeans)

#### FIG 1 ####
# read-in your data
rawdat <- read.table(file = "/Users/andyfang/Desktop/Data for M. smithii paper_2018/AntexpI_fullyr_long.csv", sep = ",", header = TRUE, fill = TRUE)

# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat

# plotting 
dat_long %>% 
  subset(weight != "0") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

dat_sum$trmt <- factor(dat_sum$trmt, levels = c("0Q30W", "1Q30W","5Q30W","10Q30W"))
pd <- position_dodge(width = 0.7)
dat_sum %>% 
  subset(weight_avg != "0") %>%
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt), alpha = 0.7) +
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.4),
                  xlim = c(0,31)) +
  scale_x_continuous(breaks = seq(0,31,5)) +
  scale_color_manual(labels = c("0Q-30W","1Q-30W","5Q-30W","10Q-30W"),
                     values = c("gray60", "#bf5700", "steelblue3", "gray0"))+
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 


# run linear mixed model
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# pairwise comparison
emmeans(mod, list(pairwise ~ trmt), adjust = "tukey")

#### FIG 2 ####
# read-in your data
rawdat <- read.table(file = "/Users/andyfang/Desktop/Data for M. smithii paper_2018/AndyAnts_ExpI.csv",
                     sep = ",", header = TRUE, fill = TRUE)

# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  subset(Queen != "0Q") %>%
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))


## plotting 
dat_long$trmt

dat_long$trmt_reord <- factor(dat_long$trmt, levels = c("1Q30W", "5Q30W","10Q30W"))

dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt_reord)) + 
  facet_wrap(~trmt_reord)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  facet_wrap(~trmt)

# ExpIII figure # It works!
dat_sum$trmt <- factor(dat_sum$trmt, levels = c("1Q30W","5Q30W","10Q30W"))
dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.5))+
  scale_color_manual(values = c("#bf5700", "steelblue3", "gray0"))+
  facet_wrap(~trmt, ncol = 3) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 


# ExpIII figure # It works! No facet!
dat_sum$trmt <- factor(dat_sum$trmt, levels = c("1Q30W","5Q30W","10Q30W"))
pd <- position_dodge(width = 0.25)
FIG2 <- dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.5)) +
  scale_color_manual(labels = c("1Q-30W","5Q-30W","10Q-30W"),
                     values = c("#bf5700", "steelblue3", "gray0")) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# change the x axis scale labels
FIG2 + scale_x_discrete(breaks=c("01","02","03","04","05","06","07","08","09","10","11","12","13"),
                        labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))

# for box plot  
  dat_long %>%
  subset(week %in% c("05", "09","13")) %>%
  ggplot() + 
  geom_boxplot(aes(x = Queen, y = weight))

# for nice and pretty plots with mean and boostraped standard error!!!
dat_long %>%
  subset(week %in% c("05")) %>% 
  group_by(Queen) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight)) %>%
  ggplot() +
  geom_point(aes(x = Queen, y = weight_avg)) +
  geom_errorbar(aes(x = Queen, ymin = weight_avg - se, ymax = weight_avg + se), width = 0.1)

## run linear mixed model
# model with only week as the random effect
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# model with both week and year as random effects
mod1 <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(mod1)

# pairwise comparison
emmeans(mod, list(pairwise ~ trmt), adjust = "tukey")


#### FIG 3 ####
# read-in your data
rawdat <- read.table(file = "https://raw.githubusercontent.com/OscarFHC/Ant_GrowthCurve/master/AndyAnts_ExpIII.csv",
                     sep = ",", header = TRUE, fill = TRUE)

# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  subset(yr != 17) %>%
  subset(Queen != "0Q") %>%
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))


## plotting 
dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

# ExpIII figure # It works!
dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.6))+
  facet_wrap(~trmt) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# ExpIII figure # It works! No facet!
pd <- position_dodge(width = 0.25)
FIG3 <- dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.6))+
  scale_color_manual(labels = c("1Q-30W","1Q-60W","1Q-90W"),
                     values = c("#bf5700", "darkolivegreen4", "gray30")) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# change the x axis scale labels
FIG3 + scale_x_discrete(breaks=c("01","02","03","04","05","06","07","08","09","10","11","12","13"),
                        labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))

## run linear mixed model
# model with only week as the random effect
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# model with both week and year as random effects
mod1 <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(mod1)
# one way to caculate the p value that includes two random effects
# not accurate, because we need to caculate the real df; we just use df =30 here
pt(3.757, 30)

# when using lmer(), we need package "lmerTest" to help us finding the p-value
install.packages("lmerTest")
library(lmerTest)
lmm <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(lmm)
anova(lmm)

# pairwise comparison
emmeans(lmm, list(pairwise ~ trmt), adjust = "tukey")

# model with both week and year as random effects
# try on 2019/02/26 --> not sure whether it' s correct or not
# is it nested?
mod2 <- lme(weight ~ trmt, random = ~1|week/yr, data = dat_long)
summary(mod2)

# model with both week and year as random effects
# try on 2019/02/26
mod2 <- lme(weight ~ trmt, random = list(yr=~1, week=~1), data = dat_long)
summary(mod2)


#### FIG 4 ####
# read-in your data
rawdat <- read.table(file = "/Users/andyfang/Desktop/Data for M. smithii paper_2018/AndyAnts_ExpV.csv",
                     sep = ",", header = TRUE, fill = TRUE)
# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))

# for nice and pretty plots with mean and boostraped standard error!!!
dat_long %>%
  group_by(Worker) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight)) %>%
  ggplot() +
  geom_point(aes(x = Worker, y = weight_avg)) +
  geom_errorbar(aes(x = Worker, ymin = weight_avg - se, ymax = weight_avg + se), width = 0.1)


## plotting 
dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

dat_sum$trmt <- factor(dat_sum$trmt, levels = c("1Q6W","1Q18W","1Q60W", "1Q90W"))

pd <- position_dodge(width = 0.7)
dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  scale_color_manual(values = c("#bf5700","#bf5700","darkolivegreen4", "gray30")) +
  coord_cartesian(ylim = c(0,0.9))+
  facet_wrap(~trmt, ncol = 2) +
  theme(legend.position = "right",
        legend.title = element_text("treatment"),
        
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# work with no facet!
pd <- position_dodge(width = 0.25)
FIG4 <- dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  scale_color_manual(labels = c("1Q-6W","1Q-18W","1Q-60W","1Q-90W"),
                     values = c("snow4","lightblue3","darkolivegreen4", "gray30")) +
  coord_cartesian(ylim = c(0,0.9))+
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_text("treatment"),
        
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey"))

# change the x axis scale labels
FIG4 + scale_x_discrete(breaks=c("01","02","03","04","05","06","07","08","09","10","11","12","13"),
                        labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))

## run linear mixed model
# model with only week as the random effect
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# pairwise comparison
emmeans(mod, list(pairwise ~ trmt), adjust = "tukey")

#### FIG 5 ####
# read-in your data
rawdat <- read.table(file = "/Users/andyfang/Desktop/Data for M. smithii paper_2018/AndyAnts_ExpVandIII.csv",
                     sep = ",", header = TRUE, fill = TRUE)
# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))

# for nice and pretty plots with mean and boostraped standard error!!!
dat_long %>%
  group_by(Worker) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight)) %>%
  ggplot() +
  geom_point(aes(x = Worker, y = weight_avg)) +
  geom_errorbar(aes(x = Worker, ymin = weight_avg - se, ymax = weight_avg + se), width = 0.1)


## plotting 
dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

dat_sum$trmt <- factor(dat_sum$trmt, levels = c("1Q60W_0.1","1Q60W_0.45","1Q90W_0.1", "1Q90W_0.45"))

# work with no facet!
pd <- position_dodge(width = 0.25)
FIG5 <- dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  scale_color_manual(labels = c("1Q-60W + 0.1g fungus ","1Q-60W + 0.45g fungus","1Q-90W + 0.1g fungus","1Q-90W + 0.45g fungus"),
                     values = c("darkolivegreen4","darkseagreen3","gray30", "gray60")) +
  coord_cartesian(ylim = c(0,0.9))+
  theme(legend.position = c(0.85, 0.9),
        legend.title = element_text("treatment"),
        
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# change the x axis scale labels
FIG5 + scale_x_discrete(breaks=c("01","02","03","04","05","06","07","08","09","10","11","12","13"),
                        labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))

# run linear mixed model
# week and year were treated as the two random effects
mod1 <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(mod1)

# when using lmer(), we need package "lmerTest" to help us finding the p-value
install.packages("lmerTest")
library(lmerTest)
lmm <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(lmm)
anova(lmm)

# pairwise comparison
emmeans(lmm, list(pairwise ~ trmt), adjust = "tukey")

#### FIG 6 ####
# read-in your data
eggraw <- read.table(file = "/Users/andyfang/Desktop/Data for M. smithii paper_2018/AndyAnts_ExpVI.csv",
                     sep = ",", header = TRUE)

# 2019/01/21 Workable code!
ggplot(eggraw, aes(x=EggNum, fill=Pool))+
  geom_histogram(binwidth = 0.5, alpha = 0.8, position = "dodge")+
  labs(y = "Number of queens",
       x = "Number of eggs laid by a single queen within 24 hours",
       fill = "Replicate run") +
  coord_cartesian(ylim = c(0,50))+
  scale_x_continuous(breaks = seq(0, 5, 1))+
  scale_fill_manual(values = c("navajowhite", "tan1", "#bf5700"))+
  theme(legend.position = c(0.8, 0.8),
        legend.direction = "horizontal", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

#ANOVA
model <- aov(EggNum ~ Pool, data=eggraw)
anova(model)
summary(model)

#Analaysis
attributes(model)
model$coefficients
TukeyHSD(model)
plot(TukeyHSD(model), las=1)

#### FIG S1 ####
# read-in your data
rawdat <- read.table(file = "https://raw.githubusercontent.com/OscarFHC/Ant_GrowthCurve/master/AndyAnts_ExpIII.csv",
                     sep = ",", header = TRUE, fill = TRUE)

# preping function for calculating SE
Se_Fun <- function (dat){
  means = c()
  for (i in 1:10000){
    means = c(means, mean(sample(dat, length(dat), replace = TRUE)))
  }
  sd(means)
}

# transforming the raw data into long format
dat_long <- rawdat %>%
  filter(yr == "18") %>%
  subset(Queen != "0Q") %>%
  gather(week, weight, -c(Queen, Worker, trmt, yr)) %>%
  subset(weight != "NA") %>%
  mutate(week = substr(week, 2, 3))


## plotting 
dat_long %>% 
  subset(weight != "NA") %>%
  ggplot + 
  geom_point(aes(x = week, y = weight)) +
  geom_smooth(aes(x = week, y = weight, group = trmt)) + 
  facet_wrap(~trmt)

dat_sum <- dat_long %>% 
  group_by(trmt, week) %>%
  summarize(weight_avg = mean(weight),
            se = Se_Fun(weight))

# ExpIII figure # It works!
dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.6))+
  facet_wrap(~trmt) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# ExpIII figure # It works! No facet!
pd <- position_dodge(width = 0.25)
FIGS1 <- dat_sum %>%  
  ggplot(aes(x = week, y = weight_avg, group = trmt)) + 
  geom_point(aes(color = trmt), size = 2) +
  geom_errorbar(aes(ymin = weight_avg - se, ymax = weight_avg + se, color = trmt), width = 0.1, position = pd) +
  geom_line(aes(color = trmt)) + 
  labs(x="Week", y="Average fungus weight (g)") +
  coord_cartesian(ylim = c(0,0.6))+
  scale_color_manual(labels = c("1Q-30W","1Q-60W","1Q-90W"),
                     values = c("#bf5700", "darkolivegreen4", "gray30")) +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_text("treatment"),
        legend.direction = "vertical", 
        panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
        panel.background =  element_blank(), axis.line = element_line(colour = "grey")) 

# change the x axis scale labels
FIGS1 + scale_x_discrete(breaks=c("01","02","03","04","05","06","07","08","09","10","11","12","13"),
                           labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"))

## run linear mixed model
# model with only week as the random effect
mod <- lme(weight ~ trmt, random = ~1|week, data = dat_long)
summary(mod)

# model with both week and year as random effects
mod1 <- lmer(weight ~ trmt + (1|week) + (1|yr), data = dat_long)
summary(mod1)

# pairwise comparison
emmeans(mod, list(pairwise ~ trmt), adjust = "tukey")
