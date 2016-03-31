# Andrew Jones
# 
# Alternation project script
#
# Clear R's Brain
rm(list=ls())
old<- Sys.time() # get start time

# Packages and Reading in Files -------------------------------------------

# Load required libraries
library(ggplot2)
library(plyr); library(dplyr)
library(tidyr)
library(RODBC)
library(gridExtra)
library(lme4)
library(nlme)

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
# This installs necessary packages and gets raw files and query from db 

# Tidying and Making New Variables --------------------------------------------

### 30/03
# Remove VJ0141 as mix up with videos on hard drive/excels/database (NOTE-changed from deleting VJ0139 on 31/03)
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="VJ0141")
# Remove what duplicates there are, for now remove the broodref (so both results) CHANGE WHEN SORTED
BroodsPerPair <- subset(BroodsPerPair, BroodRef!="48")
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="50195") #added 31/03
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="40256") #added 31/03

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
# Makes some new variables for alternation etc

# Visit Rate Difference ---------------------------------------------------

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
# This section calculates the difference between parents' visit rates and investigates how this
# affects alternation

# Investigating repeatability of alternation within a brood event ---------

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

CombinedAltAge <- select(BroodAltAge671011sp, BroodRef, alternation1, alternation2)

alt1 <- select(CombinedAltAge, BroodRef, alternation1)
alt2 <- select(CombinedAltAge, BroodRef, alternation2)

alt1 <- filter(alt1, !is.na(alternation1))
alt2 <- filter(alt2, !is.na(alternation2))

CombinedAltAge <- merge(alt1, alt2, "BroodRef")

# Plot
ggplot(CombinedAltAge, aes(x = alternation1, y = alternation2))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod5<- lm(alternation2 ~ alternation1, data=CombinedAltAge)
mod5[1]

par(mfrow=c(2,2)) 
plot(mod5)

anova(mod5)
summary(mod5)

# Output shows that there is repeatability in alternation
# F= 71.63, d.f=1,610, p<0.001
# Rsq = 0.1051

# Check without zero data
CombinedAltAgeNo0 <- filter(CombinedAltAge, alternation1>0 & alternation2>0)
ggplot(CombinedAltAgeNo0, aes(x = alternation1, y = alternation2))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_smooth(method=lm, size=1)+
  theme_classic()

mod6<- lm(alternation2 ~ alternation1, data=CombinedAltAgeNo0)
mod6[1]

par(mfrow=c(2,2)) 
plot(mod6)

anova(mod6)
summary(mod6)
# Output shows that there is still repeatability in alternation
# F= 17.12, d.f=1,547, p<0.001
# Rsq = 0.03035
# This takes early (age 6 or 7) and late (age 10 or 11) alternation and sees if they are repeatable
# within a brood event for a pair

# Investigating alternation and pair bond duration ------------------------

# Create dataframe which has PairID, BroodRef, and the alternation rate early(age6/7) and late (age10/11)
Pairbroods <- select(Merged, BroodRef, PairID)
Pairbroods <- merge(Pairbroods, CombinedAltAge, "BroodRef")
Pairbroods <- distinct(Pairbroods)

# Remove pairs that only have one brood
Pairbroods<- subset(Pairbroods,duplicated(PairID) | duplicated(PairID, fromLast=TRUE))

# Order broods by number rather than a reference
Pairbroods<- ddply(Pairbroods,.(PairID),transform,BroodNumber = rank(BroodRef,ties.method = "first"))

# Make BroodNumber a factor
Pairbroods$BroodNumber <- factor(Pairbroods$BroodNumber)

# Graph for early alternation
alt1plot<-ggplot(Pairbroods, aes(x=BroodNumber, y=alternation1, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

# Graph for late alternation
alt2plot<-ggplot(Pairbroods, aes(x=BroodNumber, y=alternation2, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(colour="none")+
  theme_classic()

# View together
grid.arrange(alt1plot, alt2plot)

# Remove zero alternation data
PairbroodsNo0 <- filter(Pairbroods, alternation1>0 & alternation2>0)

# Repeat graphs
# Graph for early alternation
alt1No0plot<-ggplot(PairbroodsNo0, aes(x=BroodNumber, y=alternation1, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(color="none")+
  theme_classic()

# Graph for late alternation
alt2No0plot<-ggplot(PairbroodsNo0, aes(x=BroodNumber, y=alternation2, colour=PairID))+
  geom_point(size=3)+
  ylim(0,1)+
  geom_line(aes(group=PairID), size=1)+
  guides(colour="none")+
  theme_classic()

# View together
grid.arrange(alt1No0plot, alt2No0plot)
# Looks at how alternation changes with brood number

# Pairwise Comparisons ----------------------------------------------------

# Calculate mean sd etc alternation for each brood event

pairsumDat<-summarise (group_by(Pairbroods, BroodNumber),
                       meanA1 = mean(alternation1),
                       meanA2 = mean(alternation2),
                       sdA1 = sd(alternation1),
                       sdA2 = sd(alternation2),
                       sample_sizeA1 = length(alternation1),
                       sample_sizeA2 = length(alternation2),
                       lower_boundA1 = meanA1 - sdA1,
                       lower_boundA2 = meanA2 - sdA2,
                       upper_boundA1 = meanA1 + sdA1,
                       upper_boundA2 = meanA2 + sdA2)

limitsA1<-aes(ymin = lower_boundA1, ymax = upper_boundA1)
limitsA2<-aes(ymin = lower_boundA2, ymax = upper_boundA2)

A1<-ggplot(pairsumDat, aes(x=BroodNumber, y=meanA1))+
  geom_bar(stat = "identity")+
  geom_errorbar(limitsA1, width = 0.1)+
  theme_classic()

A2<-ggplot(pairsumDat, aes(x=BroodNumber, y=meanA2))+
  geom_bar(stat = "identity")+
  geom_errorbar(limitsA2, width = 0.1)+
  theme_classic()

grid.arrange(A1, A2, ncol=1)

alt1mod<- lm(alternation1~BroodNumber, data=Pairbroods)

anova(alt1mod)
summary(alt1mod)

alt1tukey<-glht(alt1mod,
            linfct=mcp(BroodNumber = "Tukey"))

summary(alt1tukey)

alt2mod<- lm(alternation2~BroodNumber, data=Pairbroods)

anova(alt2mod)
summary(alt2mod)

alt2tukey<-glht(alt2mod,
                linfct=mcp(BroodNumber = "Tukey"))

summary(alt2tukey)

# Test on PairbroodsNo0
pairno0sumDat<-summarise (group_by(PairbroodsNo0, BroodNumber),
                       mean1 = mean(alternation1),
                       mean2 = mean(alternation2),
                       sd1 = sd(alternation1),
                       sd2 = sd(alternation2),
                       sample_size1 = length(alternation1),
                       sample_size2 = length(alternation2),
                       lower_bound1 = mean1 - sd1,
                       lower_bound2 = mean2 - sd2,
                       upper_bound1 = mean1 + sd1,
                       upper_bound2 = mean2 + sd2)

limits1<-aes(ymin = lower_bound1, ymax = upper_bound1)
limits2<-aes(ymin = lower_bound2, ymax = upper_bound2)

A1no0<-ggplot(pairno0sumDat, aes(x=BroodNumber, y=mean1))+
  geom_bar(stat = "identity")+
  geom_errorbar(limits1, width = 0.1)+
  theme_classic()

A2no0<-ggplot(pairno0sumDat, aes(x=BroodNumber, y=mean2))+
  geom_bar(stat = "identity")+
  geom_errorbar(limits2, width = 0.1)+
  theme_classic()

grid.arrange(A1no0, A2no0, ncol=1)

alt1mod2<- lm(alternation1~BroodNumber, data=PairbroodsNo0)

anova(alt1mod2)
summary(alt1mod2)

alt1tukey2<-glht(alt1mod2,
                linfct=mcp(BroodNumber = "Tukey"))

summary(alt1tukey2)

alt2mod2<- lm(alternation2~BroodNumber, data=PairbroodsNo0)

anova(alt2mod2)
summary(alt2mod2)

alt2tukey2<-glht(alt2mod2,
                linfct=mcp(BroodNumber = "Tukey"))

summary(alt2tukey2)




# Calculate the change in alternation between brood events
Pairbroods<- Pairbroods %>%
  group_by(PairID)%>%
  mutate(alt1difffrom1 = alternation1-first(alternation1))%>%
  mutate(alt2difffrom1 = alternation2-first(alternation2))%>%
  mutate(alt1difffromlast = c(0,diff(alternation1)))%>%
  mutate(alt2difffromlast = c(0,diff(alternation2)))

ggplot(Pairbroods, aes(x=BroodNumber, y=alt1difffrom1))+
  geom_point()+
  theme_classic()

pairsumDatdiffs<-summarise (group_by(Pairbroods, BroodNumber),
                            meanA1D1 = mean(alt1difffrom1),
                            meanA2D1 = mean(alt2difffrom1),
                            meanA1DL = mean(alt1difffromlast),
                            meanA2DL = mean(alt2difffromlast),
                            sdA1D1 = sd(alt1difffrom1),
                            sdA2D1 = sd(alt2difffrom1),
                            sdA1DL = sd(alt1difffromlast),
                            sdA2DL = sd(alt2difffromlast),
                            sample_sizeA1D1 = length(alt1difffrom1),
                            sample_sizeA2D1 = length(alt1difffrom1),
                            sample_sizeA1DL = length(alt1difffrom1),
                            sample_sizeA2DL = length(alt1difffrom1),
                            lower_boundA1D1 = meanA1D1 - sdA1D1,
                            lower_boundA2D1 = meanA2D1 - sdA2D1,
                            lower_boundA1DL = meanA1DL - sdA1DL,
                            lower_boundA2DL = meanA2DL - sdA2DL,
                            upper_boundA1D1 = meanA1D1 + sdA1D1,
                            upper_boundA2D1 = meanA2D1 + sdA2D1,
                            upper_boundA1DL = meanA1DL + sdA1DL,
                            upper_boundA2DL = meanA2DL + sdA2DL)

limitsA1D1<-aes(ymin = lower_boundA1D1, ymax = upper_boundA1D1)
limitsA2D1<-aes(ymin = lower_boundA2D1, ymax = upper_boundA2D1)
limitsA1DL<-aes(ymin = lower_boundA1DL, ymax = upper_boundA1DL)
limitsA2DL<-aes(ymin = lower_boundA2DL, ymax = upper_boundA2DL)

pairsumDatdiffs$BroodNumber <- factor(pairsumDatdiffs$BroodNumber)

A1D1<-ggplot(pairsumDatdiffs, aes(x=BroodNumber, y=meanA1D1))+
  geom_bar(stat = "identity", aes(fill=BroodNumber))+
  geom_errorbar(limitsA1D1, width = 0.1)+
  theme_classic()

A2D1<-ggplot(pairsumDatdiffs, aes(x=BroodNumber, y=meanA2D1))+
  geom_bar(stat = "identity", aes(fill=BroodNumber))+
  geom_errorbar(limitsA2D1, width = 0.1)+
  theme_classic()

A1DL<-ggplot(pairsumDatdiffs, aes(x=BroodNumber, y=meanA1DL))+
  geom_bar(stat = "identity", aes(fill=BroodNumber))+
  geom_errorbar(limitsA1DL, width = 0.1)+
  theme_classic()

A2DL<-ggplot(pairsumDatdiffs, aes(x=BroodNumber, y=meanA2DL))+
  geom_bar(stat = "identity", aes(fill=BroodNumber))+
  geom_errorbar(limitsA2DL, width = 0.1)+
  theme_classic()

grid.arrange(A1D1, A2D1, A1DL, A2DL, ncol=2)
# Continues work from previous section and makes bar charts

# Time check ------------------------
# Print elapsed time
new <- Sys.time() - old
print(new)


#- The below sections are just models I have made. Unsure if correct or if I will need/use them
#- No point running code below here
# Models (not sure necessary or correct) ----------------------------------


# Making model for EARLY alternation
modele1<-lmer(alternation1 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)

# Model diagnostics section (adapted from R course Soay Sheep model)
diagnostics <- fortify(modele1)

# Residuals vs fitted plot:
pe1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()

# Q-Q plot
pe2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()

# Put them altogether
grid.arrange(pe1, pe2, ncol=2)

# Testing the model for EARLY alternation
#### FIXED
# Is the fixed effect of Brood Number significant?
modele1<-lmer(alternation1 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modele2<-lmer(alternation1 ~               (1 + BroodNumber | PairID), data= PairbroodsNo0)
anova(modele1, modele2)
# Yes, p=0.0101

#### RANDOM
# Is the among pair variation significant?
modele1<-lmer(alternation1 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modele3<-lmer(alternation1 ~ BroodNumber + (1               | PairID), data= PairbroodsNo0)
anova(modele1, modele3, refit=FALSE) # remember refit=false because random test
# No, p=0.9877

# Is the correlation term significant?
modele1<-lmer(alternation1 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modele4<-lmer(alternation1 ~ BroodNumber + (1 + BroodNumber || PairID), data= PairbroodsNo0)
anova(modele1, modele4, refit=FALSE)
# No, p=0.8751


## Now repeat models, for LATE alternation

modell1<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)

# Model diagnostics section (adapted from R course Soay Sheep model)
diagnostics <- fortify(modell1)
# Residuals vs fitted plot:
pl1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()

# Q-Q plot
pl2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()

# Put them altogether
grid.arrange(pl1, pl2, ncol=2)

# Testing
#### FIXED
# Is the fixed effect of Brood Number significant?
modell1<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modell2<-lmer(alternation2 ~             + (1 + BroodNumber | PairID), data= PairbroodsNo0)
anova(modell1, modell2)
# No, p=0.5061

#### RANDOM
# Is the among pair variation significant?
modell1<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modell3<-lmer(alternation2 ~ BroodNumber + (1               | PairID), data= PairbroodsNo0)
anova(modell1, modell3, refit=FALSE) # remember refit=false because random test
# Yes, p=0.02461

# Is the correlation term significant?
modell1<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber | PairID), data= PairbroodsNo0)
modell4<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber || PairID), data= PairbroodsNo0)
anova(modell1, modell4, refit=FALSE)
# Yes, p=0.006489

# Testing models ----------------------------------------------------------

# Use Pairbroods
# Make PairID a factor and BroodNumber a number
Pairbroods$PairID <- factor(Pairbroods$PairID)
Pairbroods$BroodNumber <- as.numeric(Pairbroods$BroodNumber)
# Remove zero alternation data
PairbroodsNo0 <- filter(Pairbroods, alternation1>0 & alternation2>0)

# This shows how many observations for each number of broods
n_occur <- data.frame(table(PairbroodsNo0$BroodNumber))

# Need balanced for repeated measures
# From n_occur can see 57 observations where a pair has 3 broods
# Try with this to see if theres an effect
# Filter dataset
PairB3No0<-PairbroodsNo0[PairbroodsNo0$PairID %in% names(which(table(PairbroodsNo0$PairID) >= 3)), ]
PairB3No0<-filter(PairB3No0, BroodNumber<=3)
n_occur2 <- data.frame(table(PairB3No0$BroodNumber))
PairB3No0<-PairB3No0[PairB3No0$PairID %in% names(which(table(PairB3No0$PairID) == 3)), ]
# Only 49 obs as not all pairs had data for broodnumber 1 2 AND 3 (because of removal of zeros)

modeltest1<-lmer(alternation1 ~ BroodNumber + (1 | PairID), data= PairB3No0)
modeltest2<-lmer(alternation1 ~               (1 | PairID), data= PairB3No0)
anova(modeltest1,modeltest2)
# Fixed effect of brood number not significant at age6/7 as p=0.7

modeltest3<-lmer(alternation2 ~ BroodNumber + (1 | PairID), data= PairB3No0)
modeltest4<-lmer(alternation2 ~               (1 | PairID), data= PairB3No0)
anova(modeltest3,modeltest4)
# Fixed effect of brood number is significant at age 10/11 as p=0.03582
# Do a Tukey to see what happens
library(multcomp)

Tukey<-(glht(modeltest3, linfct=mcp(BroodNumber="Tukey")))
# Set margins and visualise the Tukey test
par(mfrow=c(1,1), mar = c(5,20,4,2)) # Have to set margins in base graphics, ggplot does it for you
plot(Tukey)

# To reset margins
par(mar = c(5,4,4,2))
ggplot(data=PairB3No0, aes(x=BroodNumber, y=alternation2, colour=PairID))+
  geom_point()+
  geom_line(aes(group=PairID))+
  theme_classic()

ci<-confint(lmList(alternation2~BroodNumber|PairID, data=PairB3No0), pooled=TRUE)
plot3<- plot(ci, order=1)
plot3

# Make model FOR PAIRS THAT HAVE AT LEAST 3 BROODS. ONLY COUNTS FIRST 3 BROODS.
m1<-lmer(alternation2 ~ BroodNumber + (1 + BroodNumber | PairID ), data=PairB3No0)
summary(m1)
diagnostics <- fortify(m1)

# Residuals vs fitted plot:
p1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
p1
# Q-Q plot
p2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
p2

# Is fixed effect of Brood Number important?
m1<-lmer(alternation2 ~ 1 + BroodNumber + (1 + BroodNumber | PairID ), data=PairB3No0)
m2<-lmer(alternation2 ~ 1 +               (1 + BroodNumber | PairID ), data=PairB3No0)
anova(m1,m2)
# Yes, p=0.0381

# Is the among-pair broodnumber-slope variation significant?
m1<-lmer(alternation2 ~ 1 + BroodNumber + (1 + BroodNumber | PairID ), data=PairB3No0)
m3<-lmer(alternation2 ~ 1 + BroodNumber + (1               | PairID ), data=PairB3No0)
anova(m1,m3, refit=FALSE)
# No, p=0.5722

# Is the correlation term significant?
m1<-lmer(alternation2 ~ 1 + BroodNumber + (1 + BroodNumber | PairID ), data=PairB3No0)
m4<-lmer(alternation2 ~ 1 + BroodNumber + (1 + BroodNumber || PairID ), data=PairB3No0)
anova(m1,m4, refit=FALSE)
# No, p=0.2907

ggplot(PairB3No0, aes(x=BroodNumber, y=alternation2))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()

