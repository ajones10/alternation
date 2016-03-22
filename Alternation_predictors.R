# 22/03/2016
# Mixed Model attempt
# Predictors of Alternation

# Clear R's Brain
rm(list=ls())

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

# This code reads in the data file provtest.csv
# Visit data from Issie template excels extracted by Malika
# Contains Tin Tout Sex Filename
Fullprovisioning <- read.csv("~/University/provtest.csv")

# Group data by video file (grouping by nestbox)
by_pair <-group_by(Fullprovisioning, Filename)

# For each pair calculates the number of visits (count), the number of alternations, and the
# alternation rate.
Summarydata<-summarise(by_pair,
                       count = n(),
                       number_alternations= sum(diff(Sex)!=0),
                       alternation_rate= number_alternations/count)

# Similar query to before, but this time includes more things, such as number of nestlings, time, date,
# which can be used as factors in a model.
# This code reads in the data file AJ_BroodsPerPairUpdated.csv
BroodsPerPairUpdated <- read.csv("C:/Users/Andrew Jones/SkyDrive/Documents/Git Analysis/Project testing/AJ_BroodsPerPairUpdated.csv")

# This merges the alternation summary data with the BroodsPerPairUpdated query from the database

Merged<-merge(Summarydata, BroodsPerPairUpdated, "Filename")

# Make certain columns factors
Merged$OffspringNo <- factor(Merged$OffspringNo)
Merged$Age <- factor(Merged$Age)
Merged$SocialMumID <- factor(Merged$SocialMumID)
Merged$SocialDadID <- factor(Merged$SocialDadID)

# Filter for Age 6 and 10 only
Merged <- filter(Merged, Age == 6 | Age == 10)

# Filter to remove zero alternation values
Merged<- filter(Merged, alternation_rate>0)

# need to exclude rows where only one measurement e.g Age 6 only
Merged<- subset(Merged,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

# Make a plot
ggplot(Merged, aes(x=Age, y=alternation_rate, colour=BroodRef))+
  geom_point()+
  geom_line(aes(group=BroodRef), size=1)+
  theme_classic()

# Testing effects

# Trying just Age as fixed effect
model1 <- lmer(alternation_rate ~ Age + (1 | BroodRef), data=Merged)
model2 <- lmer(alternation_rate ~ (1 | BroodRef), data=Merged)

anova(model1, model2)
# Fixed effect of age is significant as p<0.001

# Alternative method:
library(pbkrtest)
PBmodcomp(model1, model2, nsim=100)
# This gives same result (incr nsim for more accuracy) 

# Try multiple fixed effects
model3 <- lmer(alternation_rate ~ Age + OffspringNo + (1 | BroodRef), data=Merged)
model4 <- lmer(alternation_rate ~ Age +               (1 | BroodRef), data=Merged)
model5 <- lmer(alternation_rate ~       OffspringNo + (1 | BroodRef), data=Merged)

anova(model3, model4)
# Fixed effect of OffspringNo is not significant as p=0.95

anova(model3, model5)
# Age still significant (p<0.001) although different p value this time compared to mod1 vs mod2

# Try adding more fixed effects
model6 <- lmer(alternation_rate ~ Age + OffspringNo + DVDtime + (1 | BroodRef), data=Merged)
model7 <- lmer(alternation_rate ~ Age + OffspringNo +           (1 | BroodRef), data=Merged)

anova(model6, model7)
# Dvdtime has a significant effect p <0.001


