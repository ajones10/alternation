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
                       alternation_rate= (number_alternations/count),
                       alternationpercent= (alternation_rate*100))

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
conDB= odbcConnectAccess("C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\Database Copy\\Database0.74_20160322_MI\\SparrowData.mdb")

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

# Check rounded distributions
malevisitr<- ggplot(Merged, aes(x= round_male_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

femalevisitr<- ggplot(Merged, aes(x= round_female_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

visitdiffr<- ggplot(Merged, aes(x= visit_rate_diff_after_rounding))+
  geom_histogram(binwidth=1)+
  theme_classic()

grid.arrange(malevisitr, femalevisitr, visitdiffr, ncol=1)

# Calculates the Brood Number
Merged<-Merged %>%
  group_by(PairID) %>%
  mutate(BroodNumber = match(BroodRef, unique(BroodRef)))

# This changes the DVDtime to be in decimal hour format e.g 07:45 = 7.75, 12:30 = 12.5
Merged$DVDtime<-as.numeric(format(Merged$DVDtime, "%H")) +
  +     as.numeric(format(Merged$DVDtime, "%M"))/60

# Makes some new variables for alternation etc

# Visit Rate Difference ---------------------------------------------------

# Scatter plot of all the points 
ggplot(Merged, aes(x=visit_rate_diff_after_rounding, y=alternation_rate))+
  geom_point()+
  ylim(0,1)+
  xlab("Visit rate difference")+
  ylab("Alternation rate")+
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
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  ylim(0,0.8)+
  theme_classic()
# This section calculates the difference between parents' visit rates and investigates how this
# affects alternation


# Interfeed Intervals -----------------------------------------------------
# Calculate interfeed intervals
males<-filter(by_pair, Sex==1)
females<-filter(by_pair, Sex==0)

males<-males %>%
  group_by(DVDRef) %>%
  mutate(interfeed_interval = c(0,diff(TstartFeedVisit)))

females<-females %>%
  group_by(DVDRef) %>%
  mutate(interfeed_interval = c(0,diff(TstartFeedVisit)))


females<-left_join(females, Merged, by="DVDRef")
males<-left_join(males, Merged, by="DVDRef")

mfinterfeed<-bind_rows(females,males)
mfinterfeed<-group_by(mfinterfeed, DVDRef)
mfinterfeed<- select(mfinterfeed, Sex, interfeed_interval, round_male_visit_rate, round_female_visit_rate, DVDRef, BroodRef, PairID)

maleinterfeed<-filter(mfinterfeed, Sex == 1)
maleinterfeed<-select(maleinterfeed, -round_female_visit_rate)

femaleinterfeed<-filter(mfinterfeed, Sex == 0)
femaleinterfeed<-select(femaleinterfeed, -round_male_visit_rate)

# Based on distribution of the rounded visit rates, I decided to do between 3 and 14 visits.
# This results in 12 visit rates for males and females giving 144 possible outcomes

# Filter the interfeed dataframes to only those where visit rate is 3 to 14 inclusive.
femaleinterfeed314<-filter(femaleinterfeed, round_female_visit_rate <= 14 & round_female_visit_rate >= 3)
maleinterfeed314<-filter(maleinterfeed, round_male_visit_rate <= 14 & round_male_visit_rate >= 3)

# Randomise the interfeed intervals within individuals that have the same visit rate
femaleinterfeed314sh<-femaleinterfeed314 %>% group_by(round_female_visit_rate) %>% mutate(interfeed_interval=sample(interfeed_interval))
maleinterfeed314sh<-maleinterfeed314 %>% group_by(round_male_visit_rate) %>% mutate(interfeed_interval=sample(interfeed_interval))

# Example to test to start with.
# Make df for female rate = 5, and assign 4 rows to a "Simulated Female"
femaleinterfeedsim5<-filter(femaleinterfeed314sh, round_female_visit_rate == 5)
femaleinterfeedsim5<-mutate(femaleinterfeedsim5, SimFemale = rep(1:((nrow(femaleinterfeedsim5)/4)+1), each = 4, len = nrow(femaleinterfeedsim5)))
# Shuffle the simulated female ID
femaleinterfeedsim5<-mutate(femaleinterfeedsim5, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
femaleinterfeedsim5<-femaleinterfeedsim5%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

# Need to do this for each sex and all visit rates 3 - 14 (24 times in total)
# Females first
#SimFemale3 means a dataframe of all simulated females visiting at rate 3

SimFemale3<-filter(femaleinterfeed314sh, round_female_visit_rate == 3)
SimFemale3<-mutate(SimFemale3, SimFemale = rep(1:((nrow(SimFemale3)/2)+1), each = 2, len = nrow(SimFemale3)))
# Shuffle the simulated female ID
SimFemale3<-mutate(SimFemale3, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale3<-SimFemale3%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale4<-filter(femaleinterfeed314sh, round_female_visit_rate == 4)
SimFemale4<-mutate(SimFemale4, SimFemale = rep(1:((nrow(SimFemale4)/3)+1), each = 3, len = nrow(SimFemale4)))
# Shuffle the simulated female ID
SimFemale4<-mutate(SimFemale4, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale4<-SimFemale4%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale5<-filter(femaleinterfeed314sh, round_female_visit_rate == 5)
SimFemale5<-mutate(SimFemale5, SimFemale = rep(1:((nrow(SimFemale5)/4)+1), each = 4, len = nrow(SimFemale5)))
# Shuffle the simulated female ID
SimFemale5<-mutate(SimFemale5, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale5<-SimFemale5%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale6<-filter(femaleinterfeed314sh, round_female_visit_rate == 6)
SimFemale6<-mutate(SimFemale6, SimFemale = rep(1:((nrow(SimFemale6)/5)+1), each = 5, len = nrow(SimFemale6)))
# Shuffle the simulated female ID
SimFemale6<-mutate(SimFemale6, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale6<-SimFemale6%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale7<-filter(femaleinterfeed314sh, round_female_visit_rate == 7)
SimFemale7<-mutate(SimFemale7, SimFemale = rep(1:((nrow(SimFemale7)/6)+1), each = 6, len = nrow(SimFemale7)))
# Shuffle the simulated female ID
SimFemale7<-mutate(SimFemale7, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale7<-SimFemale7%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale8<-filter(femaleinterfeed314sh, round_female_visit_rate == 8)
SimFemale8<-mutate(SimFemale8, SimFemale = rep(1:((nrow(SimFemale8)/7)+1), each = 7, len = nrow(SimFemale8)))
# Shuffle the simulated female ID
SimFemale8<-mutate(SimFemale8, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale8<-SimFemale8%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale9<-filter(femaleinterfeed314sh, round_female_visit_rate == 9)
SimFemale9<-mutate(SimFemale9, SimFemale = rep(1:((nrow(SimFemale9)/8)+1), each = 8, len = nrow(SimFemale9)))
# Shuffle the simulated female ID
SimFemale9<-mutate(SimFemale9, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale9<-SimFemale9%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale10<-filter(femaleinterfeed314sh, round_female_visit_rate == 10)
SimFemale10<-mutate(SimFemale10, SimFemale = rep(1:((nrow(SimFemale10)/9)+1), each = 9, len = nrow(SimFemale10)))
# Shuffle the simulated female ID
SimFemale10<-mutate(SimFemale10, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale10<-SimFemale10%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale11<-filter(femaleinterfeed314sh, round_female_visit_rate == 11)
SimFemale11<-mutate(SimFemale11, SimFemale = rep(1:((nrow(SimFemale11)/10)+1), each = 10, len = nrow(SimFemale11)))
# Shuffle the simulated female ID
SimFemale11<-mutate(SimFemale11, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale11<-SimFemale11%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale12<-filter(femaleinterfeed314sh, round_female_visit_rate == 12)
SimFemale12<-mutate(SimFemale12, SimFemale = rep(1:((nrow(SimFemale12)/11)+1), each = 11, len = nrow(SimFemale12)))
# Shuffle the simulated female ID
SimFemale12<-mutate(SimFemale12, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale12<-SimFemale12%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale13<-filter(femaleinterfeed314sh, round_female_visit_rate == 13)
SimFemale13<-mutate(SimFemale13, SimFemale = rep(1:((nrow(SimFemale13)/12)+1), each = 12, len = nrow(SimFemale13)))
# Shuffle the simulated female ID
SimFemale13<-mutate(SimFemale13, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale13<-SimFemale13%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

SimFemale14<-filter(femaleinterfeed314sh, round_female_visit_rate == 14)
SimFemale14<-mutate(SimFemale14, SimFemale = rep(1:((nrow(SimFemale14)/13)+1), each = 13, len = nrow(SimFemale14)))
# Shuffle the simulated female ID
SimFemale14<-mutate(SimFemale14, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale14<-SimFemale14%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

# Males next
#SimMale3 means a dataframe of all simulated males visiting at rate 3

SimMale3<-filter(maleinterfeed314sh, round_male_visit_rate == 3)
SimMale3<-mutate(SimMale3, SimMale = rep(1:((nrow(SimMale3)/2)+1), each = 2, len = nrow(SimMale3)))
# Shuffle the simulated female ID
SimMale3<-mutate(SimMale3, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale3<-SimMale3%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale4<-filter(maleinterfeed314sh, round_male_visit_rate == 4)
SimMale4<-mutate(SimMale4, SimMale = rep(1:((nrow(SimMale4)/3)+1), each = 3, len = nrow(SimMale4)))
# Shuffle the simulated female ID
SimMale4<-mutate(SimMale4, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale4<-SimMale4%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale5<-filter(maleinterfeed314sh, round_male_visit_rate == 5)
SimMale5<-mutate(SimMale5, SimMale = rep(1:((nrow(SimMale5)/4)+1), each = 4, len = nrow(SimMale5)))
# Shuffle the simulated female ID
SimMale5<-mutate(SimMale5, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale5<-SimMale5%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale6<-filter(maleinterfeed314sh, round_male_visit_rate == 6)
SimMale6<-mutate(SimMale6, SimMale = rep(1:((nrow(SimMale6)/5)+1), each = 5, len = nrow(SimMale6)))
# Shuffle the simulated female ID
SimMale6<-mutate(SimMale6, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale6<-SimMale6%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale7<-filter(maleinterfeed314sh, round_male_visit_rate == 7)
SimMale7<-mutate(SimMale7, SimMale = rep(1:((nrow(SimMale7)/6)+1), each = 6, len = nrow(SimMale7)))
# Shuffle the simulated female ID
SimMale7<-mutate(SimMale7, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale7<-SimMale7%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale8<-filter(maleinterfeed314sh, round_male_visit_rate == 8)
SimMale8<-mutate(SimMale8, SimMale = rep(1:((nrow(SimMale8)/7)+1), each = 7, len = nrow(SimMale8)))
# Shuffle the simulated female ID
SimMale8<-mutate(SimMale8, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale8<-SimMale8%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale9<-filter(maleinterfeed314sh, round_male_visit_rate == 9)
SimMale9<-mutate(SimMale9, SimMale = rep(1:((nrow(SimMale9)/8)+1), each = 8, len = nrow(SimMale9)))
# Shuffle the simulated female ID
SimMale9<-mutate(SimMale9, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale9<-SimMale9%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale10<-filter(maleinterfeed314sh, round_male_visit_rate == 10)
SimMale10<-mutate(SimMale10, SimMale = rep(1:((nrow(SimMale10)/9)+1), each = 9, len = nrow(SimMale10)))
# Shuffle the simulated female ID
SimMale10<-mutate(SimMale10, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale10<-SimMale10%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale11<-filter(maleinterfeed314sh, round_male_visit_rate == 11)
SimMale11<-mutate(SimMale11, SimMale = rep(1:((nrow(SimMale11)/10)+1), each = 10, len = nrow(SimMale11)))
# Shuffle the simulated female ID
SimMale11<-mutate(SimMale11, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale11<-SimMale11%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale12<-filter(maleinterfeed314sh, round_male_visit_rate == 12)
SimMale12<-mutate(SimMale12, SimMale = rep(1:((nrow(SimMale12)/11)+1), each = 11, len = nrow(SimMale12)))
# Shuffle the simulated female ID
SimMale12<-mutate(SimMale12, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale12<-SimMale12%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale13<-filter(maleinterfeed314sh, round_male_visit_rate == 13)
SimMale13<-mutate(SimMale13, SimMale = rep(1:((nrow(SimMale13)/12)+1), each = 12, len = nrow(SimMale13)))
# Shuffle the simulated female ID
SimMale13<-mutate(SimMale13, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale13<-SimMale13%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

SimMale14<-filter(maleinterfeed314sh, round_male_visit_rate == 14)
SimMale14<-mutate(SimMale14, SimMale = rep(1:((nrow(SimMale14)/13)+1), each = 13, len = nrow(SimMale14)))
# Shuffle the simulated female ID
SimMale14<-mutate(SimMale14, SimMale = sample(SimMale))
# Calculate cumulative sum for each SimFemale
SimMale14<-SimMale14%>%
  group_by(SimMale)%>%
  mutate(MaleCumulative = cumsum(interfeed_interval))

# Join together by Sex
SimulatedMales<-bind_rows(list(SimMale3, SimMale4, SimMale5, SimMale6, SimMale7, SimMale8, SimMale9, SimMale10, SimMale11, SimMale12, SimMale13, SimMale14))
SimulatedFemales<-bind_rows(list(SimFemale3, SimFemale4, SimFemale5, SimFemale6, SimFemale7, SimFemale8, SimFemale9, SimFemale10, SimFemale11, SimFemale12, SimFemale13, SimFemale14))

# Select necessary rows and rename
SimulatedMales<- rename(SimulatedMales, VisitRate = round_male_visit_rate)
SimulatedMales<- rename(SimulatedMales, SimID = SimMale)
SimulatedMales<- rename(SimulatedMales, Interval = MaleCumulative)
SimulatedMales<- select(SimulatedMales, SimID, Sex, VisitRate, Interval)

SimulatedFemales<- rename(SimulatedFemales, VisitRate = round_female_visit_rate)
SimulatedFemales<- rename(SimulatedFemales, SimID = SimFemale)
SimulatedFemales<- rename(SimulatedFemales, Interval = FemCumulative)
SimulatedFemales<- select(SimulatedFemales, SimID, Sex, VisitRate, Interval)

SimulatedData<- bind_rows(list(SimulatedMales, SimulatedFemales))
# This orders/arranges df by visit rate, then by SimID, then by interval
SimulatedData<-SimulatedData%>%
  group_by(VisitRate, SimID)%>%
  arrange(Interval)








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
  xlab("Alternation score for age 6/7")+
  ylab("Alternation score for age 10/11")+
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
# Then make a bar chart for alternation and brood number
# Anova and tukey

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


# Repeat on PairbroodsNo0 (removes zero alternation)
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

#
#
#
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
# This looks at 

# Models that include lots of things! -------------------------------------


fullmod<-lmer(alternation_rate ~ 1 + BroodNumber + Age + DVDtime + OffspringNo + visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=Merged)
summary(fullmod)

diagnosticsfullmod <- fortify(fullmod)

# Residuals vs fitted plot:
p1fullmod<- ggplot(diagnosticsfullmod, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
p1fullmod
# Q-Q plot
p2fullmod<-ggplot(diagnosticsfullmod, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
p2fullmod

# Can see from Resid vs Fitted plot that there is a diagonal line 
# possibly due to the zero alternation data
# Try removing those and see what happens

MergedNo0 <- filter(Merged, alternation_rate>0)
# Removing zeros removed 115 observations, leaving 1521
# Rerun model above with new dataset

fullmodNo0<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
summary(fullmodNo0)

diagnosticsfullmodNo0 <- fortify(fullmodNo0)

# Residuals vs fitted plot:
p1fullmodNo0<- ggplot(diagnosticsfullmodNo0, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
p1fullmodNo0
# Q-Q plot
p2fullmodNo0<-ggplot(diagnosticsfullmodNo0, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
p2fullmodNo0


# Testing fixed effects
# Brood Number
fullmodNo0<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
fullmodNo0bn<-lmer(alternationpercent ~ 1 +             Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
anova(fullmodNo0, fullmodNo0bn)

# Age
fullmodNo0<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
fullmodNo0age<-lmer(alternationpercent ~ 1 + BroodNumber +    OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
anova(fullmodNo0, fullmodNo0age)

# OffspringNo
fullmodNo0<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
fullmodNo0offspringno<-lmer(alternationpercent ~ 1 + BroodNumber + Age +    DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
anova(fullmodNo0, fullmodNo0offspringno)

# DVD Time
# Because of missing DVDtimes need to make a subset to test this one
MergedNo0Dvdtime <- filter(MergedNo0, !is.na(DVDtime))
fullmodNo0DVDTimefull<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0Dvdtime)
fullmodNo0DVDTime<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + 
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0Dvdtime)
anova(fullmodNo0DVDTimefull, fullmodNo0DVDTime)

# Visit Rate Difference
fullmodNo0<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                   visit_rate_diff_after_rounding + (1 | PairID) + (1 | BroodRef), data=MergedNo0)
fullmodNo0vrd<-lmer(alternationpercent ~ 1 + BroodNumber + Age + OffspringNo + DVDtime +
                                                    (1 | PairID) + (1 | BroodRef), data=MergedNo0)
anova(fullmodNo0, fullmodNo0vrd)
# Time check ------------------------
# Print elapsed time
new <- Sys.time() - old
print(new)