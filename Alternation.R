##########
# Andrew Jones
# 
# Alternation project script
#
##########

# Clear R's Brain
rm(list=ls())

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RODBC)
library(gridExtra)

# Read in the excel file with raw data
# Currently this code reads in the data file provtest.csv
# Contains visit data from Issie template excels extracted by Malika
# Contains Tin Tout Sex Filename
Fullprovisioning <- read.csv("~/University/provtest.csv")

# Group data by video file (grouping by nestbox)
by_pair <-group_by(Fullprovisioning, Filename)

# For each pair calculates the number of visits (count), the number of alternations, and the
# alternation rate.
Summarydata<-summarise(by_pair,
                       count = n(),
                       malecount = length(Sex[Sex==1]),
                       femalecount = length(Sex[Sex==0]),
                       number_alternations= sum(diff(Sex)!=0),
                       alternation_rate= number_alternations/count)

# Plots the alternation rates as a histogram
ggplot(Summarydata, aes(x=alternation_rate))+
  geom_histogram(binwidth=0.05, col="grey")+
  theme_classic()

# Now need to include extra data from database
# Make a query in the database and then get the SQL code

#### Next 3 lines need looking at- probably need to change this query to consider everything
# This query created in db which gets info for broods where social parents are certain, and situation=4 (chicks)
# Looks at female as focal parent so that data is not duplicated
# Provisioning videos at days 6, 7, 10, 11 only for consistency

## Get BroodsPerPairUpdated query from database
conDB= odbcConnectAccess("C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\Database Copy\\Database0.74_20160310_MI\\SparrowData.mdb")

BroodsPerPair <- sqlQuery(conDB, "
                          SELECT tblBroods.SocialMumID, tblBroods.SocialDadID, tblBroods.SocialMumCertain, tblBroods.SocialDadCertain, tblBroods.BroodRef, tblDVDInfo.Situation, tblDVDInfo.DVDRef, tblDVDInfo.DVDNumber, tblDVD_XlsFiles.Filename, tblDVDInfo.Age, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.OffspringNo, tblParentalCare.EffectTime
FROM ((tblBroods INNER JOIN tblDVDInfo ON tblBroods.BroodRef = tblDVDInfo.BroodRef) INNER JOIN tblDVD_XlsFiles ON tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef) INNER JOIN tblParentalCare ON (tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef) AND (tblDVDInfo.DVDRef = tblParentalCare.DVDRef)
                          WHERE (((tblBroods.SocialMumCertain)=Yes) AND ((tblBroods.SocialDadCertain)=Yes) AND ((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.Age)=6 Or (tblDVDInfo.Age)=7 Or (tblDVDInfo.Age)=10 Or (tblDVDInfo.Age)=11));")
close(conDB) # closes connection to db 

# Merge the alternation summary data with the BroodsPerPair query from the database:

Merged<-merge(Summarydata, BroodsPerPair, "Filename") # merging by 'Filename'

# Creating new Columns
# This creates a "PairID" 
Merged <- transform(Merged, PairID = as.numeric(interaction(SocialMumID, SocialDadID, drop=TRUE)))

# Create "visit rate" for both Male and Female
# Per hour to standardise
# Visit rate per hour = Number of visits/Effect time * 60
Merged <- transform(Merged, male_visit_rate = (malecount/EffectTime)*60)
Merged <- transform(Merged, female_visit_rate = (femalecount/EffectTime)*60)
Merged <- transform(Merged, visit_rate_difference = abs(male_visit_rate - female_visit_rate))

# Check distributions
malevisit<- ggplot(Merged, aes(x= male_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

femalevisit<- ggplot(Merged, aes(x= female_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

visitdiff<- ggplot(Merged, aes(x= visit_rate_difference))+
  geom_histogram(binwidth=1)+
  theme_classic()

grid.arrange(malevisit, femalevisit, visitdiff, ncol=1)

# Faceted plots 
ggplot(Merged, aes(x= male_visit_rate, fill=Age))+
  geom_histogram(binwidth=1)+
  facet_wrap(~Age, ncol=2)+
  theme_classic()

ggplot(Merged, aes(x= female_visit_rate, fill=Age))+
  geom_histogram(binwidth=1)+
  facet_wrap(~Age, ncol=2)+
  theme_classic()

ggplot(Merged, aes(x= visit_rate_difference, fill=Age))+
  geom_histogram(binwidth=1)+
  facet_wrap(~Age, ncol=2)+
  theme_classic()

# Scatter plot
ggplot(Merged, aes(x=visit_rate_difference, y=alternation_rate))+
  geom_point()+
  theme_classic()

# Round the visit rates to nearest whole number to get whole number differences
# Create new columns
Merged <- transform(Merged, round_male_visit_rate = round(male_visit_rate))
Merged <- transform(Merged, round_female_visit_rate = round(female_visit_rate))
Merged <- transform(Merged, visit_rate_diff_after_rounding = abs(round_male_visit_rate - round_female_visit_rate))

# Scatter plot of all the points 
ggplot(Merged, aes(x=visit_rate_diff_after_rounding, y=alternation_rate))+
  geom_point()+
  theme_classic()

# This will summarise the data by the difference in visit rate, giving mean alternation etc
# Enables a graph to be plotted with error bars
VisitRateSum<-summarise(group_by(Merged, visit_rate_diff_after_rounding),
                        meanalternation = mean(alternation_rate),
                        SDalt= sd(alternation_rate),
                        SampleSize= length(alternation_rate),
                        SE= SDalt/sqrt(SampleSize),
                        lwr= meanalternation-SE,
                        upr= meanalternation+SE)

# Plots the mean alternation for each difference in rate
ggplot(VisitRateSum, aes(x=visit_rate_diff_after_rounding, y=meanalternation))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lwr, ymax=upr))+
  theme_classic()

####################################################################################
#
#
##### Investigating repeatability of alternation within a brood event
#
#
####################################################################################
#
# Below section needs looking at again (as of 24/03) to check right things being used
#

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
#
## 22/03/2016
# Sample size here has 153 observations where there is non-zero alternation for age 6 and age 10
# Only covers 2012, 2013, 2014. Lots more data available
# Repeat this once rest has been distracted and compare results.
