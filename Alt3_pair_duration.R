###
##
# 22/03/2016
# Alternation Script 3 - Looking at pair bonds
##
###

# Beginning section same as Alternation.R up to line 77

# Clear R's Brain
rm(list=ls())

# Load required libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(gridExtra)

# This code reads in the data file provtest.csv
# Visit data from Issie template excels extracted by Malika
# Contains Tin Tout Sex Filename
Fullprovisioning <- read.csv("~/University/provtest.csv")

# Group data by video file (grouping by nestbox)
by_pair <-group_by(Fullprovisioning, Filename)

# For each pair calculates the number of visits (count), the number of alternations, and the
# alternation rate.
Summarydata<-dplyr::summarise(by_pair,
                       count = n(),
                       number_alternations= sum(diff(Sex)!=0),
                       alternation_rate= number_alternations/count)


# This code reads in the data file AJ_BroodsPerPair.csv
BroodsPerPair <- read.csv("C:/Users/Andrew Jones/SkyDrive/Documents/Git Analysis/Project testing/AJ_BroodsPerPair.csv")

# This merges the alternation summary data with the BroodsPerPair query from the database

Merged<-merge(Summarydata, BroodsPerPair, "Filename")

# This creates a "PairID" 
Merged <- transform(Merged, PairID = as.numeric(interaction(SocialMumID, SocialDadID, drop=TRUE)))

# Factors
Merged$PairID <- factor(Merged$PairID)
Merged$Age <- factor(Merged$Age)

# 6 and 10 only

MergedD6D10 <- filter(Merged, Age == 6 | Age == 10)

# need to exclude rows where only one measurement e.g Age 6 only
MergedD6D10<- subset(MergedD6D10,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

# Try age 6 only and age 10 only separately
MergedD6 <- filter(MergedD6D10, Age == 6)
MergedD10 <- filter(MergedD6D10, Age == 10)

# Exclude observations where pair only has one brood
MergedD6<- subset(MergedD6,duplicated(PairID) | duplicated(PairID, fromLast=TRUE))
MergedD10<- subset(MergedD10,duplicated(PairID) | duplicated(PairID, fromLast=TRUE))

# Repeat graph
ggplot(MergedD6, aes(x=BroodRef, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  geom_line(aes(group=PairID), size=1)+
  theme_classic()

# Now need to work out how to standardize x axis, eg Brood Number
MergedD6<- ddply(MergedD6,.(PairID),transform,BroodNumber = rank(BroodRef,ties.method = "first"))
MergedD10<- ddply(MergedD10,.(PairID),transform,BroodNumber = rank(BroodRef,ties.method = "first"))

# Repeat graph
Age6Plot<-ggplot(MergedD6, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

Age10Plot<-ggplot(MergedD10, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

grid.arrange(Age6Plot, Age10Plot)

# Removing zero alternation data
MergedD6No0 <- filter(MergedD6, alternation_rate>0)
MergedD10No0 <- filter(MergedD10, alternation_rate>0)

# Repeat graphs
Age6No0Plot<-ggplot(MergedD6No0, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

Age10No0Plot<-ggplot(MergedD10No0, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

grid.arrange(Age6No0Plot, Age10No0Plot)

#####
####
## Mixed model for Day 6 ONLY
####
#####

model6.1 <- lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD6No0)

# Model diagnostics section (adapted from R course Soay Sheep model)
diagnostics <- fortify(model6.1)

# Residuals vs fitted plot:
p1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()

# Q-Q plot
p2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()

# Resids vs Fits divided up by PairID
p3<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  facet_wrap(~PairID, ncol=5)+
  theme_classic()

# Put them altogether
grid.arrange(p1, p2, p3, ncol=2)

# Testing the model
#### FIXED
# Is the fixed effect of Brood Number significant?
model6.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD6No0)
model6.2<-lmer(alternation_rate ~               (1 + BroodNumber | PairID), data= MergedD6No0)
anova(model6.1, model6.2)
# No, p=0.265

#### RANDOM
# Is the among pair variation significant?
model6.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD6No0)
model6.3<-lmer(alternation_rate ~ BroodNumber + (1               | PairID), data= MergedD6No0)
anova(model6.1, model6.3, refit=FALSE) # remember refit=false because random test
# No, p=0.971

# Is the correlation term significant?
model6.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD6No0)
model6.4<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber || PairID), data= MergedD6No0)
anova(model6.1, model6.4, refit=FALSE)
# No, p=0.8

############
## Now repeat models, for Age 10
############

ci2 <- confint(lmList(alternation_rate ~ BroodNumber | PairID, MergedD10No0), pooled = TRUE)
plot(ci2, order = 1)

model10.1 <- lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD10No0)

# Model diagnostics section (adapted from R course Soay Sheep model)
diagnostics <- fortify(model10.1)
# Residuals vs fitted plot:
p10.1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()

# Q-Q plot
p10.2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()

# Resids vs Fits divided up by PairID
p10.3<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  facet_wrap(~PairID, ncol=5)+
  theme_classic()

# Put them altogether
grid.arrange(p10.1, p10.2, p10.3, ncol=2)

# Testing
#### FIXED
# Is the fixed effect of Brood Number significant?
model10.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD10No0)
model10.2<-lmer(alternation_rate ~               (1 + BroodNumber | PairID), data= MergedD10No0)
anova(model10.1, model10.2)
# No, p=0.9635

#### RANDOM
# Is the among pair variation significant?
model10.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD10No0)
model10.3<-lmer(alternation_rate ~ BroodNumber + (1               | PairID), data= MergedD10No0)
anova(model10.1, model10.3, refit=FALSE) # remember refit=false because random test
# No, p=0.8412

# Is the correlation term significant?
model10.1<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber | PairID), data= MergedD10No0)
model10.4<-lmer(alternation_rate ~ BroodNumber + (1 + BroodNumber || PairID), data= MergedD10No0)
anova(model10.1, model10.4, refit=FALSE)
# No, p=0.5565