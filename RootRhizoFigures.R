# Plant health data for generational experimental design.
#Joanna Colovas 04/06/2022

#importing the correct libraries for the script and ANOVA analyses
library(tidyverse)
library(multcompView)

#import dataset into R
RootHealth <- read_csv("~/OneDrive - Michigan State University/20220406_Root_Metadata_Health.csv")

#filter entire dataset to Gen 1 vs Gen 2
Gen1 <- filter(RootHealth, generation == 1)
Gen1

Gen2 <- filter(RootHealth, generation == 2)
Gen2

#Plot colors array
ConditionColors <- c("#0197F6", "#F9A602", "#03AC13")
#Gen2Colors <- c("#0197F6","#0197F6","#0197F6", "#FFE800","#FFE800","#FFE800", "#03AC13", "#03AC13","#03AC13")
#Gen1Conditions <- c("Control", "Drought", "Nutrient")
#Gen2Conditions <- c("Control", "Control", "Control", "Drought", "Drought", "Drought", "Nutrient", "Nutrient", "Nutrient")

#set working directory
setwd("~/OneDrive - Michigan State University/RootRhizoFigures")

##SHOOT##

#shoot G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
ShootGen1ANOVA <- aov(dry.shoot.weight_g ~ condition, data = Gen1)
summary(ShootGen1ANOVA)
#Tukey Test for differences
ShootGen1Tukey <- TukeyHSD(ShootGen1ANOVA)
print(ShootGen1Tukey)
#add letters for significance, same lettered groups are alike
ShootSigDiffs1 <-  multcompLetters4(ShootGen1ANOVA, ShootGen1Tukey)
print(ShootSigDiffs1)
ShootSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, dry.shoot.weight_g)) %>% 
  arrange(desc(mean))
print(ShootSigDiffsTable1)
ShootSigDiffs1 <- as.data.frame.list(ShootSigDiffs1$condition)
ShootSigDiffsTable1$ShootSigDiffs1 <- ShootSigDiffs1
print(ShootSigDiffsTable1)

#shoot G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
ShootGen2ANOVA <- aov(dry.shoot.weight_g ~ combocondition, data = Gen2)
summary(ShootGen2ANOVA)
#Tukey Test for differences
ShootGen2Tukey <- TukeyHSD(ShootGen2ANOVA)
print(ShootGen2Tukey)
#add letters for significance
ShootSigDiffs2 <-  multcompLetters4(ShootGen2ANOVA, ShootGen2Tukey)
print(ShootSigDiffs2)
ShootSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, dry.shoot.weight_g)) %>% 
  arrange(desc(mean))
print(ShootSigDiffsTable2)
ShootSigDiffs2 <- as.data.frame.list(ShootSigDiffs2$combocondition)
ShootSigDiffsTable2$ShootSigDiffs2 <- ShootSigDiffs2
print(ShootSigDiffsTable2)

#Shoot Biomass gen 1 and gen 2 boxplots

ShootGen1Biomass <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = dry.shoot.weight_g, color = condition))  + 
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Shoot Biomass", 
        y= "Dry Shoot Weight(g)",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=ShootSigDiffsTable1, aes(condition, 4, label = ShootSigDiffs1$Letters))
ShootGen1Biomass
ggsave("ShootGen1Biomass.png", width = 5, height = 5, dpi = 1000)

ShootGen2Biomass <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = dry.shoot.weight_g, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Shoot Biomass", 
        y= "Dry Shoot Weight(g)",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=ShootSigDiffsTable2, aes(combocondition, 1, label = ShootSigDiffs2$Letters))
ShootGen2Biomass
ggsave("ShootGen2Biomass.png", width = 5, height = 5, dpi = 1000)

##ROOT##

#Root G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
RootGen1ANOVA <- aov(dry.root.weight_g ~ condition, data = Gen1)
summary(RootGen1ANOVA)
#Tukey Test for differences
RootGen1Tukey <- TukeyHSD(RootGen1ANOVA)
print(ShootGen1Tukey)
#add letters for significance, same lettered groups are alike
RootSigDiffs1 <-  multcompLetters4(RootGen1ANOVA, RootGen1Tukey)
print(RootSigDiffs1)
RootSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, dry.root.weight_g)) %>% 
  arrange(desc(mean))
print(RootSigDiffsTable1)
RootSigDiffs1 <- as.data.frame.list(RootSigDiffs1$condition)
RootSigDiffsTable1$RootSigDiffs1 <- RootSigDiffs1
print(RootSigDiffsTable1)

#shoot G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
RootGen2ANOVA <- aov(dry.root.weight_g ~ combocondition, data = Gen2)
summary(RootGen2ANOVA)
#Tukey Test for differences
RootGen2Tukey <- TukeyHSD(RootGen2ANOVA)
print(RootGen2Tukey)
#add letters for significance
RootSigDiffs2 <-  multcompLetters4(RootGen2ANOVA, RootGen2Tukey)
print(RootSigDiffs2)
RootSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, dry.root.weight_g)) %>% 
  arrange(desc(mean))
print(RootSigDiffsTable2)
RootSigDiffs2 <- as.data.frame.list(RootSigDiffs2$combocondition)
RootSigDiffsTable2$RootSigDiffs2 <- RootSigDiffs2
print(RootSigDiffsTable2)


#Root Biomass gen 1 and gen 2 boxplots
RootGen1Biomass <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = dry.root.weight_g, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Root Biomass", 
        y= "Dry Root Weight(g)",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=RootSigDiffsTable1, aes(condition, 2, label = RootSigDiffs1$Letters))
RootGen1Biomass
ggsave("RootGen1Biomass.png", width = 5, height = 5, dpi = 1000)

RootGen2Biomass <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = dry.root.weight_g, color = condition)) +
scale_color_manual(values = ConditionColors,
                   name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Root Biomass", 
        y= "Dry Root Weight(g)",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=RootSigDiffsTable2, aes(combocondition, 0.5, label = RootSigDiffs2$Letters))
RootGen2Biomass
ggsave("RootGen2Biomass.png", width = 5, height = 5, dpi = 1000)

##POD NUMBER##

#Pod num G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
PodGen1ANOVA <- aov(pod_number ~ condition, data = Gen1)
summary(PodGen1ANOVA)
#Tukey Test for differences
PodGen1Tukey <- TukeyHSD(PodGen1ANOVA)
print(PodGen1Tukey)
#add letters for significance, same lettered groups are alike
PodSigDiffs1 <-  multcompLetters4(PodGen1ANOVA, PodGen1Tukey)
print(PodSigDiffs1)
PodSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, pod_number)) %>% 
  arrange(desc(mean))
print(PodSigDiffsTable1)
PodSigDiffs1 <- as.data.frame.list(PodSigDiffs1$condition)
PodSigDiffsTable1$PodSigDiffs1 <- PodSigDiffs1
print(PodSigDiffsTable1)

#pod count G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
PodGen2ANOVA <- aov(pod_number ~ combocondition, data = Gen2)
summary(PodGen2ANOVA)
#Tukey Test for differences
PodGen2Tukey <- TukeyHSD(PodGen2ANOVA)
print(PodGen2Tukey)
#add letters for significance
PodSigDiffs2 <-  multcompLetters4(PodGen2ANOVA, PodGen2Tukey)
print(PodSigDiffs2)
PodSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, pod_number)) %>% 
  arrange(desc(mean))
print(PodSigDiffsTable2)
PodSigDiffs2 <- as.data.frame.list(PodSigDiffs2$combocondition)
PodSigDiffsTable2$PodSigDiffs2 <- PodSigDiffs2
print(PodSigDiffsTable2)

#PodNumber  gen 1 and 2 box plots
PodNumGen1 <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = pod_number, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Pod Count", 
        y= "Pod Count",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=PodSigDiffsTable1, aes(condition, 2, label = PodSigDiffs1$Letters))
PodNumGen1
ggsave("PodNumGen1.png", width = 5, height = 5, dpi = 1000)

PodNumGen2 <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = pod_number, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Pod Count", 
        y= "Pod Count",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=PodSigDiffsTable2, aes(combocondition, 0.5, label = PodSigDiffs2$Letters))
PodNumGen2
ggsave("PodNumGen2.png", width = 5, height = 5, dpi = 1000)

##SEED NUMBER##

#seed num G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
SeedGen1ANOVA <- aov(seed_number ~ condition, data = Gen1)
summary(SeedGen1ANOVA)
#Tukey Test for differences
SeedGen1Tukey <- TukeyHSD(SeedGen1ANOVA)
print(SeedGen1Tukey)
#add letters for significance, same lettered groups are alike
SeedSigDiffs1 <-  multcompLetters4(SeedGen1ANOVA, SeedGen1Tukey)
print(SeedSigDiffs1)
SeedSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, seed_number)) %>% 
  arrange(desc(mean))
print(SeedSigDiffsTable1)
SeedSigDiffs1 <- as.data.frame.list(SeedSigDiffs1$condition)
SeedSigDiffsTable1$SeedSigDiffs1 <- SeedSigDiffs1
print(SeedSigDiffsTable1)

#pod count G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
SeedGen2ANOVA <- aov(seed_number ~ combocondition, data = Gen2)
summary(SeedGen2ANOVA)
#Tukey Test for differences
SeedGen2Tukey <- TukeyHSD(SeedGen2ANOVA)
print(SeedGen2Tukey)
#add letters for significance
SeedSigDiffs2 <-  multcompLetters4(SeedGen2ANOVA, SeedGen2Tukey)
print(SeedSigDiffs2)
SeedSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, seed_number)) %>% 
  arrange(desc(mean))
print(SeedSigDiffsTable2)
SeedSigDiffs2 <- as.data.frame.list(SeedSigDiffs2$combocondition)
SeedSigDiffsTable2$SeedSigDiffs2 <- SeedSigDiffs2
print(SeedSigDiffsTable2)

#Seed Number  gen 1 and 2 box plots
SeedNumGen1 <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = seed_number, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Seed Count", 
        y= "Seed Count",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=SeedSigDiffsTable1, aes(condition, 25, label = SeedSigDiffs1$Letters))
SeedNumGen1
ggsave("SeedNumGen1.png", width = 5, height = 5, dpi = 1000)

SeedNumGen2 <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = pod_number, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Seed Count", 
        y= "Seed Count",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=SeedSigDiffsTable2, aes(combocondition, 0.5, label = SeedSigDiffs2$Letters))
SeedNumGen2
ggsave("SeedNumGen2.png", width = 5, height = 5, dpi = 1000)

##STOMATAL CONDUCTANCE##

#Stomatal conductance G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
StoConGen1ANOVA <- aov(stomatal_conductance ~ condition, data = Gen1)
summary(StoConGen1ANOVA)
#Tukey Test for differences
StoConGen1Tukey <- TukeyHSD(StoConGen1ANOVA)
print(StoConGen1Tukey)
#add letters for significance, same lettered groups are alike
StoConSigDiffs1 <-  multcompLetters4(StoConGen1ANOVA, StoConGen1Tukey)
print(StoConSigDiffs1)
StoConSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, stomatal_conductance)) %>% 
  arrange(desc(mean))
print(StoConSigDiffsTable1)
StoConSigDiffs1 <- as.data.frame.list(StoConSigDiffs1$condition)
StoConSigDiffsTable1$StoConSigDiffs1 <- StoConSigDiffs1
print(StoConSigDiffsTable1)

#stomatal conductance G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
StoConGen2ANOVA <- aov(stomatal_conductance ~ combocondition, data = Gen2)
summary(StoConGen2ANOVA)
#Tukey Test for differences
StoConGen2Tukey <- TukeyHSD(StoConGen2ANOVA)
print(StoConGen2Tukey)
#add letters for significance
StoConSigDiffs2 <-  multcompLetters4(StoConGen2ANOVA, StoConGen2Tukey)
print(StoConSigDiffs2)
StoConSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, stomatal_conductance)) %>% 
  arrange(desc(mean))
print(StoConSigDiffsTable2)
StoConSigDiffs2 <- as.data.frame.list(StoConSigDiffs2$combocondition)
StoConSigDiffsTable2$StoConSigDiffs2 <- StoConSigDiffs2
print(StoConSigDiffsTable2)

#Stomatal conductance  gen 1 and 2 box plots
StoConGen1 <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = stomatal_conductance, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Stomatal Conductance ", 
        y= "Stomatal Conductance (mol/m^2s)",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=StoConSigDiffsTable1, aes(condition, 0, label = StoConSigDiffs1$Letters))
StoConGen1
ggsave("StoConGen1.png", width = 5, height = 5, dpi = 1000)

StoConGen2 <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = stomatal_conductance, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Stomatal Conductance", 
        y= "Stomatal Conductance (mol/m^2s)",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=StoConSigDiffsTable2, aes(combocondition, 0, label = StoConSigDiffs2$Letters))
StoConGen2
ggsave("StoConGen2.png", width = 5, height = 5, dpi = 1000)


##PHOTOSYNTHETIC RATE##

#photosynthetic rate G1 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
PhotoGen1ANOVA <- aov(photosynthetic_rate ~ condition, data = Gen1)
summary(PhotoGen1ANOVA)
#Tukey Test for differences
PhotoGen1Tukey <- TukeyHSD(PhotoGen1ANOVA)
print(PhotoGen1Tukey)
#add letters for significance, same lettered groups are alike
PhotoSigDiffs1 <-  multcompLetters4(PhotoGen1ANOVA, PhotoGen1Tukey)
print(PhotoSigDiffs1)
PhotoSigDiffsTable1 <- group_by(Gen1,  condition ) %>% 
  summarize( mean = mean(na.rm = TRUE, photosynthetic_rate)) %>% 
  arrange(desc(mean))
print(PhotoSigDiffsTable1)
PhotoSigDiffs1 <- as.data.frame.list(PhotoSigDiffs1$condition)
PhotoSigDiffsTable1$PhotoSigDiffs1 <- PhotoSigDiffs1
print(PhotoSigDiffsTable1)

#photosynthetic rate G2 differences by ANOVA, and differences between groups by Tukey test
#ANOVA test 1 way 
PhotoGen2ANOVA <- aov(photosynthetic_rate ~ combocondition, data = Gen2)
summary(PhotoGen2ANOVA)
#Tukey Test for differences
PhotoGen2Tukey <- TukeyHSD(PhotoGen2ANOVA)
print(PhotoGen2Tukey)
#add letters for significance
PhotoSigDiffs2 <-  multcompLetters4(PhotoGen2ANOVA, PhotoGen2Tukey)
print(PhotoSigDiffs2)
PhotoSigDiffsTable2 <- group_by(Gen2, combocondition ) %>% 
  summarize( mean = mean(na.rm = TRUE, photosynthetic_rate)) %>% 
  arrange(desc(mean))
print(PhotoSigDiffsTable2)
PhotoSigDiffs2 <- as.data.frame.list(PhotoSigDiffs2$combocondition)
PhotoSigDiffsTable2$StoConSigDiffs2 <- PhotoSigDiffs2
print(PhotoSigDiffsTable2)

#photosynthetic rate gen 1 and 2 box plots
PhotoGen1 <- ggplot(data = Gen1, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = condition, y = photosynthetic_rate, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Condition") +
  labs (title = "Generation 1 Photosynthetic Rate ", 
        y= "Photosynthetic Rate (µmol/ m^2s)",
        x = "Experimental Condition\n N=5 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=PhotoSigDiffsTable1, aes(condition, 0, label = PhotoSigDiffs1$Letters))
PhotoGen1
ggsave("PhotoGen1.png", width = 5, height = 5, dpi = 1000)

PhotoGen2 <- ggplot(data = Gen2, show.legend= TRUE) + 
  geom_boxplot(na.rm = TRUE, mapping = aes(x = combocondition, y = photosynthetic_rate, color = condition)) +
  scale_color_manual(values = ConditionColors,
                     name = "Parent Plant Condition\nDuring Generation 1" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (title = "Generation 2 Photosynthetic Rate", 
        y= "Photosynthetic Rate (µmol/ m^2s)",
        x = "Compounded Experimental Condition\n N=12 per group",
        subtitle = "Letters indicate significant differences at p < 0.01\n1-way ANOVA, Tukey's Test") + 
  geom_text(data=StoConSigDiffsTable2, aes(combocondition, 0, label = StoConSigDiffs2$Letters))
PhotoGen2
ggsave("PhotoGen2.png", width = 5, height = 5, dpi = 1000)

