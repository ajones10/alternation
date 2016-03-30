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

# Read in the excel file with raw data R_RawFeedingVisits.csv
# Contains DVDRef TstartFeedVisit TendFeedVisit Sex
# Sex=0 for female, =1 for male
Fullprovisioning <- read.csv("C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\R_RawFeedingVisits.csv")

# Group data by video file (grouping by nestbox)
by_pair <-group_by(Fullprovisioning, DVDRef)

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
                          WHERE (((tblBroods.SocialMumCertain)=Yes) AND ((tblBroods.SocialDadCertain)=Yes) AND ((tblDVDInfo.Situation)=4));")
close(conDB) # closes connection to db 

### 30/03
# There is a duplicate. Two Age6 videos for Brood Ref 1190, so remove VJ0139 which does not match with
# what is in the database.
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="VJ0139")
# Remove what duplicates there are, for now remove the broodref (so both results) CHANGE WHEN SORTED
BroodsPerPair <- subset(BroodsPerPair, BroodRef!="48")
BroodsPerPair <- subset(BroodsPerPair, BroodRef!="53")
BroodsPerPair <- subset(BroodsPerPair, BroodRef!="529")

# Merge the alternation summary data with the BroodsPerPair query from the database:

Merged<-merge(Summarydata, BroodsPerPair, "DVDRef") # merging by 'DVDRef'

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

# Data comes in pairs of Age6 and Age10, or 7 and 11. Do each separately:

# 6 and 10

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
# F= 21.72, d.f=1,356, p<0.001
# Rsq = 0.0575  - still a very large amount of unexplained variation

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

## Output this time: F=3.689, d.f=1,324, p=0.05564, Rsq=0.01126
# Removing zeros makes data normally distributed
# No longer any significant repeatability
# Other factors?

# 7 and 11

MergedD7D11 <- filter(Merged, Age == 7 | Age == 11)


# need to exclude rows where only one measurement e.g Age 7 only
MergedD7D11<- subset(MergedD7D11,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

# Repeat graph
ggplot(MergedD7D11, aes(x=Age, y=alternation_rate, colour=BroodRef))+
  geom_point(size=3)+
  geom_line(aes(group=BroodRef), size=1)+
  theme_classic()

####
# Select() the columns alternation brood ref and age may solve some issues 

BroodAltAge711<- select(MergedD7D11, BroodRef, Age, alternation_rate)

#Spread?
BroodAltAge711<- spread(BroodAltAge711, Age, alternation_rate)

#Rename age columns
BroodAltAge711<- plyr::rename(BroodAltAge711, c("7" = "Age7", "11" = "Age11"))
View(BroodAltAge711)

# Plot
ggplot(BroodAltAge711, aes(x = Age7, y = Age11))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod3<- lm(Age11 ~ Age7, data=BroodAltAge711)
mod3[1]

par(mfrow=c(2,2)) 
plot(mod3)

anova(mod3)
summary(mod3)

## 
#
# Output shows that there is repeatability in alternation
# F= 47.95, d.f=1,246, p<0.001
# Rsq = 0.1631  - still a large amount of unexplained variation, but this is much bigger than D6D11

#### This next section addresses the right skew in the data
### 
# Though not sure if necessary? (May change when previous and later years data is added)
# Could remove all observations where alternation is 0

BroodAltAge711No0 <- filter(BroodAltAge711, Age7>0 & Age11>0)

# Repeat Plot and model like before
ggplot(BroodAltAge711No0, aes(x = Age7, y = Age11))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod4<- lm(Age11 ~ Age7, data=BroodAltAge711No0)
mod4[1]

par(mfrow=c(2,2)) 
plot(mod4)

anova(mod4)
summary(mod4)

## Output this time: F=14.69, d.f=1,216, p=0.0001665, Rsq=0.06366
# Removing zeros makes data normally distributed
# Still significant repeatability

# Next need to try doing Alternation 1 as age 6 or 7, and Alternation 2 as age 10 or 11

Merged671011 <- filter(Merged, Age == 6 | Age == 10 | Age == 7 | Age == 11)

# need to exclude rows where only one measurement e.g Age 6 only
Merged671011<- subset(Merged671011,duplicated(BroodRef) | duplicated(BroodRef, fromLast=TRUE))

####
# Select() the columns alternation brood ref and age may solve some issues 

BroodAltAge671011<- select(Merged671011, BroodRef, Age, alternation_rate)

BroodAltAge671011 <- mutate(BroodAltAge671011, alternation1 = Age==6 | Age==7)
BroodAltAge671011sp <- spread(BroodAltAge671011, alternation1, alternation_rate)
BroodAltAge671011sp<- plyr::rename(BroodAltAge671011sp, c("TRUE" = "alternation1", "FALSE" = "alternation2"))
