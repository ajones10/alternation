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
library(dplyr)
library(tidyr)

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

# Plots the alternation rates as a histogram
ggplot(Summarydata, aes(x=alternation_rate))+
  geom_histogram(binwidth=0.05, col="grey")+
  theme_classic()

#####
#
# More work, looking at broods
# 2012 to 2014 (inclusive) only 
#####

# Query created in db which gets info for broods where social parents are certain, and situation=4 (chicks)
# Looks at female as focal parent so that data is not duplicated
# Provisioning videos at days 6, 7, 10, 11 only for consistency
# although this is indirect, probably quicker way to get it straight from db
# This code reads in the data file AJ_BroodsPerPair.csv
BroodsPerPair <- read.csv("C:/Users/Andrew Jones/SkyDrive/Documents/Git Analysis/Project testing/AJ_BroodsPerPair.csv")

# This merges the alternation summary data with the BroodsPerPair query from the database

Merged<-merge(Summarydata, BroodsPerPair, "Filename")

###
# Now going to try and see, for one pair, if anything happens across brood events
# MumID= 4576 and DadID= 4682 (selected because have a number of differnet broods)

Pair1 <- filter(Merged, SocialMumID==4576 & SocialDadID==4682)

# Exclude rows where only one measurement e.g Age 6 only
Pair1<- subset(Pair1,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

# Plot a graph to see what happens
ggplot(Pair1, aes(x=BroodRef, y=alternation_rate, colour=Age))+
  geom_point()+
  geom_line(aes(group=Age))+
  theme_classic()

# Make a model
Pair1$BroodRef <- factor(Pair1$BroodRef)

pair1mod<- lm(alternation_rate ~ BroodRef, data=Pair1)
par(mfrow=c(2,2))
plot(pair1mod)

anova(pair1mod)
summary(pair1mod)

library(multcomp) # Installs library for Tukey
Tukey<-(glht(pair1mod, linfct=mcp(BroodRef="Tukey")))
summary(Tukey)

