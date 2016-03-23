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


# Repeat graph
ggplot(MergedD6D10, aes(x=BroodRef, y=alternation_rate, colour=Age))+
  geom_point(size=3)+
  geom_line(aes(group=Age), size=1)+
  facet_wrap(~PairID)+
  theme_classic()

# Try day 6 only and day 10 only separately
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
ggplot(MergedD6, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  geom_line(aes(group=PairID), size=1)+
  theme_classic()

ggplot(MergedD10, aes(x=BroodNumber, y=alternation_rate, colour=PairID))+
  geom_point(size=3)+
  geom_line(aes(group=PairID), size=1)+
  theme_classic()
