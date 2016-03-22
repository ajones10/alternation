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
# 21/03/2016
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

# This looks to have worked! 

##### Investigating repeatability of alternation within a brood event

# Now to create graph where x=age (day of video) y=alternation grouped by pair
ggplot(Merged, aes(x=Age, y=alternation_rate, colour=BroodRef))+
  geom_point(size=3)+
  geom_line(aes(group=BroodRef), size=1)+
  theme_classic()

# Can see from this graph nearly every one has day 6 and day 10 so filter dataframe to reflect this

MergedD6D10 <- filter(Merged, Age == 6 | Age == 10)

# need to exclude rows where only one measurement e.g Age 6 only
MergedD6D10<- subset(MergedD6D10,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

# Repeat graph
ggplot(MergedD6D10, aes(x=Age, y=alternation_rate, colour=BroodRef))+
  geom_point(size=3)+
  geom_line(aes(group=BroodRef), size=1)+
  theme_classic()

####
# Select() the columns alternation brood ref and age may solve some issues 

BroodAltAge<- select(MergedD6D10, BroodRef, Age, alternation_rate)

#Spread?
BroodAltAge<- spread(BroodAltAge, Age, alternation_rate)

#Rename age columns
BroodAltAge<- plyr::rename(BroodAltAge, c("6" = "Age6", "10" = "Age10"))
View(BroodAltAge)

# Plot
ggplot(BroodAltAge, aes(x = Age6, y = Age10))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod1<- lm(Age10 ~ Age6, data=BroodAltAge)
mod1[1]

par(mfrow=c(2,2)) 
plot(mod1)

anova(mod1)
summary(mod1)

## 
#
# Output shows that there is repeatability in alternation
# F= 6.901, d.f=1,172, p=0.009
# Rsq = 0.038  - still a very large amount of unexplained variation

#### This next section addresses the right skew in the data
### 
# Though not sure if necessary? (May change when previous and later years data is added)
# Could remove all observations where alternation is 0

BroodAltAgeNo0 <- filter(BroodAltAge, Age6>0 & Age10>0)
View(BroodAltAgeNo0)

# Repeat Plot and model like before
ggplot(BroodAltAgeNo0, aes(x = Age6, y = Age10))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod2<- lm(Age10 ~ Age6, data=BroodAltAgeNo0)
mod2[1]

par(mfrow=c(2,2)) 
plot(mod2)

anova(mod2)
summary(mod2)

## Output this time: F=2.405, d.f=1,151, p=0.123, Rsq=0.0157
# Removing zeros makes data normally distributed
# No longer any significant repeatability
# Other factors?
