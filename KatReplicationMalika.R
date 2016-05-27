#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  Andrew Jones  
#	 Analyse provisioning data sparrows
#	 Start : 15/04/2015
#	 last modif : 12/05/2016  
#	 commit: ANOVA/Tukey and more complete predictors model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

{### packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)
library(lme4)
library(gridExtra)
}


{### Get raw data (from source() or R_output folder)

# source('COMPILATION_PROVISIONING.R')
# or :

output_folder <- "C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\Output_files"
MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/"))
MY_tblBroods <- read.csv(paste(output_folder,"R_MY_tblBroods.csv", sep="/"))
MY_tblDVDInfo <- read.csv(paste(output_folder,"R_MY_tblDVDInfo.csv", sep="/"))
MY_RawFeedingVisits <- read.csv(paste(output_folder,"R_MY_RawFeedingVisits.xlsx", sep="/"))

}

head(MY_tblBroods) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
head(MY_tblDVDInfo) # metadata for all analysed videos
head(MY_tblParentalCare) # summary stats for all analyzed videos
head(MY_RawFeedingVisits,32) # OF directly followed by IN are merged feeding visits ; will be used for simulation

{### select valid video files for studying behavioural compatibility in chick provisioning

nrow(MY_tblBroods) # 1886
nrow(MY_tblDVDInfo) # 2112
nrow(MY_tblParentalCare) # 2112
nrow(MY_RawFeedingVisits) # 61940


list_non_valid_DVDRef <- 
  c(MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0],# 6 - where 0 chicks
    MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5],# 906 - where still brooding (age <=5)
    MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& !is.na(MY_tblParentalCare$DVDRef)], # 171 - one sex did not visit
    MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 - both parents unidentified
    MY_tblParentalCare$DVDRef[is.na(MY_tblParentalCare$EffectiveTime)]) # 9 files with no visits at all

list_non_valid_DVDRef <- list_non_valid_DVDRef[!is.na(list_non_valid_DVDRef)]

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]

}

{### sample sizes

nrow(MY_tblParentalCare) # 1768 DVD files
length(unique(MY_tblDVDInfo$BroodRef)) # 958 broods videotaped at least once
mean(table(MY_tblDVDInfo$BroodRef)) # on average 1.8 videos per brood watched
range(table(MY_tblDVDInfo$BroodRef)) # range from 1 to 3

}

# Create MyTable
# Add together M and F visit rates together
MY_tblParentalCare<- mutate(MY_tblParentalCare, MFVisitRate= MVisit1RateH + FVisit1RateH)


a<-select(MY_tblDVDInfo, DVDRef, BroodRef, DVDInfoChickNb, ChickAge, RelTimeMins)
b<-select(MY_tblParentalCare, DVDRef, DiffVisit1Rate, MFVisitRate, AlternationValue)
c<-select(MY_tblBroods, BroodRef, Nb3, NbHatched, NestboxRef, BreedingYear, DadAge, MumAge, ParentsAge, SocialMumID, SocialDadID, PairID, PairBroodNb, AvgMass, AvgTarsus)
MyTable<- left_join(a, b, by= "DVDRef")
MyTable<- left_join(MyTable, c, by="BroodRef")


############################################ 
# replication Bebbington & Hatchwell study #
############################################

{### simulation alternation with Kat's method
  
  {## Description of Kat’ simulation described in the paper page 3 + supp figure and my point of view on it
    
    # Alternation score fore observed nest watches:
    # A = F/ (t-1)
    # with F the number of alternation and t the number of feeding visits
    # for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding visits is 17 (if one hour was watched).
    
    # Simulation steps:
    # 1) select the provisioning rates for individuals of either sexes that are not too infrequent (remove the extreme low and high values of x or y)
    # 2) extract all interfeed intervals for all individuals of a same provisioning rate and of a same sex and randomize them
    # 3) (see supp fig) create all combinations of female and male with provisioning rate x and y by sampling without replacement x-1 female interfeed intervals and y-1 male interfeed intervals. Several simulated nest watches are possible per each provisioning rate combination, depending on the number of individuals with these provisioning rates that were observed . Simulated nest watches are created until one of the pool of interfeed intervals per each provisioning rate per sex is empty. Each pool of interfeed intervals, for instance the one for female with provisioning rate x, are reuse for each combination involving x.
    # 4) (see supp fig) the cumulative sum of interfeed interval are calculated for each sex separately, and then rows are merged and sorted 
    
    # 5) Alternation score for those simulated nest watch is then calculated with the formula:
    # A = F/ (t-1)
    # with F the number of alternation and t the number of feeding interval
    # for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding intervals is 15.
    
    # 6) within each combination of provisioning rate combination, 10000 bootstraps of alternation scores were ran (this is what the paper says, but in fact the code Kat sent shows that the bootstrapping was made at a latter stage, see below)
    # 7) All simulated alternation scores were pooled into groups of visit rate differences (absolute value of x minus y) [and the bootstrapping happened here in Kat’s code – but maybe it does not matter]
    # 8) All observed alternation scores were also pooled into groups of visit rate differences and compared to the simulated one leading to Fig. 1.
    
    # My point on view on this:
    # 1) the selection seems arbitrary, if anything, I suggest we remove the extreme 5% quantile ?
    # 2) what about non-independence of some nest watches because of same MID or FID or PairID ? Is it ok because they will then be more represented in both observed and simulated nest watches ? But their individuality is anyway disrupted, for instance for a same provisioning rate, a bird can be consistently very regular or can be very irregular. I think this argues for randomizing within nest watch.
    # 3) what if we randomly select several large intervals and spill over the hour ? Also, in our case, we also calculated visit rate per hour although we typically record for 90 min, so we maybe have more variance in interfeed intervals to select from when picking ‘visit rate-1’ interfeed intervals.
    # 4) (see supp fig) intervals length are considered like the starting time of feeding visits, and like if the first visit of the male and the female were both at time zero. I think maybe it is better again to shuffle intervals within a file, keeping the firs time start for each sex in this file.
    # 5) the formula to calculate A is different for observed and simulated nest watches I believe, and the maximum A that can be obtained for a same combination of x and y is systematically lower for the simulated ones as soon as the provisioning rate difference (absolute value of x minus y) is larger than 1 (see excel file attached).
    # 8) and 1) 
    # for simulation : selection of individuals that have a provisioning rate not extreme, then pooled into visit rate differences, only with pairs whose individuals have non extreme provisioning rate.
    # in observed values: we take every individuals, every combinations, so for a same visit rate difference, individuals can have very extreme provisioning rates. I think observed combinations where one of the sex has an extreme value for provisioning rate should be remove for comparison with simulated data.
    
    # I don’t know how this impact Kat’s results, but I will now try to run simulations within files because I can better argue for it, and maybe compare both outputs to let her know.
    
  }
  
  
  {## calculation top 5% of feeding rates for each sex 
  
  summary(MY_tblParentalCare$FVisit1RateH)
  summary(MY_tblParentalCare$MVisit1RateH)
  dev.new()
  par(mfrow=c(2,1)) 
  hist(MY_tblParentalCare$FVisit1RateH, xlim=c(0,50), ylim = c(0,1000))
  hist(MY_tblParentalCare$MVisit1RateH, xlim=c(0,50), ylim = c(0,1000))
  
  quantile(MY_tblParentalCare$FVisit1RateH[!is.na(MY_tblParentalCare$FVisit1RateH)], c(0.05,0.95))
  quantile(MY_tblParentalCare$MVisit1RateH[!is.na(MY_tblParentalCare$MVisit1RateH)], c(0.05,0.95))
  }
  
  {## Get all simulated combinations of individuals with specific provisioning rates, and calculate their alternation
  
  {# Create RawInterfeeds and split per sex and select provisioning rates from 3 to 22
    RawInterfeeds <- merge(x= MY_RawFeedingVisits[,c('DVDRef','Sex','Interval')], y=MY_tblParentalCare[,c('DVDRef','MVisit1RateH', 'FVisit1RateH','DiffVisit1Rate','AlternationValue')] , by='DVDRef', all.x=TRUE)
    
    MRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','MVisit1RateH')], RawInterfeeds$Sex == 1)
    MRawInterfeeds322 <- MRawInterfeeds[MRawInterfeeds$MVisit1RateH >=3 & MRawInterfeeds$MVisit1RateH <=22,]
    FRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','FVisit1RateH')], RawInterfeeds$Sex == 0)
    FRawInterfeeds322 <- FRawInterfeeds[FRawInterfeeds$FVisit1RateH >=3 & FRawInterfeeds$FVisit1RateH <=22,]
  }
  
  {# Randomise the interfeed intervals within individuals of the same sex that have the same visit rate
  FShuffledInterfeeds322 <- FRawInterfeeds322[-1] %>% group_by(FVisit1RateH) %>% mutate(Interval=sample(Interval))
  MShuffledInterfeeds322 <- MRawInterfeeds322[-1] %>% group_by(MVisit1RateH) %>% mutate(Interval=sample(Interval))
  }
  
  {# create one simulated df per sex per visit rate, with shuffled intervals associated to a SimID of length 'visit rate - 1'
  
  SimFemale <- list ()
  
  for (i in 3:22)
  {
    # one group of visit rate at a time
    SimFemale[[i]] <- filter(FShuffledInterfeeds322, FVisit1RateH == i)
    # add SimID to (visit rate - 1) visits
    SimFemale[[i]] <- mutate(SimFemale[[i]], SimID = rep(1:((nrow(SimFemale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimFemale[[i]])))
    # Shuffle the SimID
    SimFemale[[i]]<-mutate(SimFemale[[i]], SimID = sample(SimID)) # sample without replacement
    # sort (not needed but easier to look at output)
    SimFemale[[i]]<-arrange(SimFemale[[i]],SimID)
    # Calculate cumulative sum for each SimID
    SimFemale[[i]]<-SimFemale[[i]]%>%
      group_by(SimID)%>%
      mutate(CumInt = cumsum(Interval))
    
  }
  
  
  SimMale <- list ()
  
  for (i in 3:22)
  {
    # one group of visit rate at a time
    SimMale[[i]] <- filter(MShuffledInterfeeds322, MVisit1RateH == i)
    # add SimID to (visit rate - 1) visits
    SimMale[[i]] <- mutate(SimMale[[i]], SimID = rep(1:((nrow(SimMale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimMale[[i]])))
    # Shuffle the SimID
    SimMale[[i]]<-mutate(SimMale[[i]], SimID = sample(SimID)) # sample without replacement
    # sort
    SimMale[[i]]<-arrange(SimMale[[i]],SimID)
    # Calculate cumulative sum for each SimID
    SimMale[[i]]<-SimMale[[i]]%>%
      group_by(SimID)%>%
      mutate(CumInt = cumsum(Interval))
    
  }
  
  
  SimMale <- do.call(rbind,SimMale)
  SimFemale <- do.call(rbind,SimFemale)
  SimData <- bind_rows(SimMale, SimFemale) # different from rbind as it binds two df with different columns, adding NAs
  SimData[is.na(SimData)] <- 0
  }
  
  head(SimData)
  
  {# create MiFj: 400 dataframe of combine male visit * female visit rate
  # all individuals of one sex of one visit rate are reused for each combination involving this visit rate
  
  MiFj <- list()
  i = rep(3:22, each = 20) # male visit rate
  j = rep((3:22), 20) # female visit rate
  
  for (k in 1:400) # 400 combination
  { 
    MiFj[[k]]<-SimData%>%
      filter(MVisit1RateH==i[k] | FVisit1RateH==j[k])%>%
      arrange(SimID, CumInt) 
  }
  
  AllMiFj <- do.call(rbind, MiFj)
  nrow(AllMiFj) # 1075820     #Andrew gets 972800
  }
  
  {# add running OverallSimID and select combinations with both sex, with the full number of intervals for a given provisioning rates
  AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1))) # shift all SimID from 1, get a running number changing at each mismatch between the original vector of SimID and the shifted one
  AllMiFj$Sex <- as.numeric(as.character(AllMiFj$Sex))
  
  AllMiFj_splitperOverallSimID <- split(AllMiFj, AllMiFj$OverallSimID)
  
  AllMiFj_splitperOverallSimID_fun <- function(x){
    return(c(
      length(x$Sex[x$Sex==0]), 
      length(x$Sex[x$Sex==1])
    ))
  }
  
  AllMiFj_splitperOverallSimID_out1 <- lapply(AllMiFj_splitperOverallSimID,FUN= AllMiFj_splitperOverallSimID_fun )
  AllMiFj_splitperOverallSimID_out2 <- data.frame(rownames(do.call(rbind,AllMiFj_splitperOverallSimID_out1)),do.call(rbind, AllMiFj_splitperOverallSimID_out1))
  
  rownames(AllMiFj_splitperOverallSimID_out2 ) <- NULL
  colnames(AllMiFj_splitperOverallSimID_out2 ) <- c('OverallSimID','NbF', 'NbM')
  
  # remove all OverSimID where one sex not present
  AllMiFj <- AllMiFj[ ! AllMiFj$OverallSimID %in% AllMiFj_splitperOverallSimID_out2$OverallSimID[AllMiFj_splitperOverallSimID_out2$NbF == 0 | AllMiFj_splitperOverallSimID_out2$NbM == 0] ,]
  nrow(AllMiFj) # 778147 - Andrew gets 709714
  
  # rename OverallSimID to have it continuous
  AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1)))
  
  # write.table(AllMiFj, file = "AllMiFj.xls", col.names=TRUE, sep='\t') # 20160412
  }
  
  head(AllMiFj)
  
  {# calculate alternation for each combination of individuals with specific provisioning rates
  
  FinalMiFj <- group_by(AllMiFj,OverallSimID)
  
  SimulatedSummaryKat <- summarise(FinalMiFj,
                                   tt = n(), # what we have here are interfeeds > if we want numbers of feeds add 2 ?							
                                   F = sum(diff(Sex)!=0),
                                   A = round((F/(tt-1))*100,2),# what we have are interfeeds > ?
                                   MVisitRate = max(MVisit1RateH),## added this for bootstrapping per category - this allows removing lines with 0 ?
                                   FVisitRate = max(FVisit1RateH),## added this for bootstrapping per category
                                   MFVisitRate = paste(max(MVisit1RateH),max(FVisit1RateH), sep="-"), ## added this for bootstrapping per category
                                   VisitRateDifference= abs(max(MVisit1RateH)-max(FVisit1RateH)))
  
  
  tail(as.data.frame(SimulatedSummaryKat),60)				
  freqCombination <- arrange(count(SimulatedSummaryKat, MFVisitRate), n)
  nrow(freqCombination) # 400 combinations
  
  }
  
  }
  
  head(SimulatedSummaryKat)
  
  PreBootstrapKat<- group_by(SimulatedSummaryKat, VisitRateDifference)
  PreBootstrapSummaryKat <- summarise(PreBootstrapKat,
                                      Amean = mean(A),
                                      StdDev = sd(A),
                                      n= length(A))
  {## bootstrap A from SimulatedSummaryKat within each visit rate difference
  
  samplemean <- function(x, d) {return(mean(x[d]))}
  
  Aboot <- data.frame(matrix(,data=NA, nrow=20, ncol=4))
  colnames(Aboot) <- c('VisitRateDifference','Amean','Alower','Aupper')
  Aboot$VisitRateDifference <- c(0:19)
  
  for (i in 1:20)
  {
    Aboot$Amean[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$t0
    Aboot$Alower[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[2] 
    Aboot$Aupper[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[3] 
  }
  }
  
  Aboot$n <- 10000
  Aboot
  
  {# summary Aobserved
  # per visit rate difference like in the paper
  MY_tblParentalCare_forA <- MY_tblParentalCare[!is.na(MY_tblParentalCare$AlternationValue),]
  MY_tblParentalCare_perVisitRateDiff <- group_by(MY_tblParentalCare_forA, DiffVisit1Rate)
  
  Summary_MY_tblParentalCare_perVisitRateDiff <- summarise (MY_tblParentalCare_perVisitRateDiff,
                                                            Amean = mean(AlternationValue),
                                                            StdDev = sd(AlternationValue),
                                                            Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
                                                            Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
                                                            n = length(AlternationValue))
  
  Summary_MY_tblParentalCare_perVisitRateDiff20 <- Summary_MY_tblParentalCare_perVisitRateDiff[1:20,]
  Summary_MY_tblParentalCare_perVisitRateDiff20 <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff20,VisitRateDifference= DiffVisit1Rate)
  
  }
  
  Summary_MY_tblParentalCare_perVisitRateDiff20
  ObservedSummary<-select(Summary_MY_tblParentalCare_perVisitRateDiff20, VisitRateDifference, Amean, StdDev, n)
  
  ObservedSummary$Type <- "Observed"
  PreBootstrapSummaryKat$Type <- "Expected"
  
  TTestData<- rbind(ObservedSummary, PreBootstrapSummaryKat)

# T test formula using summary data
  
  # m1, m2: the sample means
  # s1, s2: the sample standard deviations
  # n1, n2: the same sizes
  # m0: the null value for the difference in means to be tested for. Default is 0. 
  # equal.variance: whether or not to assume equal variance. Default is FALSE. 
  t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
  {
    if( equal.variance==FALSE ) 
    {
      se <- sqrt( (s1^2/n1) + (s2^2/n2) )
      # welch-satterthwaite df
      df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
    } else
    {
      # pooled standard deviation, scaled by the sample sizes
      se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
      df <- n1+n2-2
    }      
    t <- (m1-m2-m0)/se 
    dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
    names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
    return(dat) 
  }
  
  t.test2(70.84855,51.05297,14.408150,15.470304,138,2525)
  t.test2(69.14567,50.78700,12.663135,14.993340,254,4748)
  t.test2(26.07000,12.99492 ,14.047780,4.314166 , 10,59 )
  {# combine observed and expected and plot
  
  # per visit rate difference like in the paper
  Summary_MY_tblParentalCare_perVisitRateDiff20$Type <- "Observed"
  Aboot$Type <- "Expected"
  
  VisitRateDiff_Amean <- rbind(Aboot, Summary_MY_tblParentalCare_perVisitRateDiff20)
  
  Fig1 <- ggplot(data=VisitRateDiff_Amean, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
    geom_point(size=4)+
    geom_line(size=1.5, alpha=0.7)+
    geom_errorbar(aes(ymin=Alower, ymax=Aupper), size=1.5)+
    xlab("Visit rate difference")+
    ylab("Mean alternation")+
    scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "Mean Observed"))+
    scale_x_continuous(breaks = pretty(VisitRateDiff_Amean$VisitRateDifference, n = 12)) +
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0, 100)) +
    theme_classic(base_size=20)+
    theme(
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
  
  }
  
}

Fig1


{### simulation alternation: shuffling intervals within files where both sex visit at least once

## I think it could be still interesting to remove extreme values of provisioning rate (not normal to have just one visit, or 50...)
## I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

RawFeedingVisitsBothSexes <- MY_RawFeedingVisits[,c('DVDRef','TstartFeedVisit','Sex','Interval')]
RawFeedingVisitsBothSexes$Sex <- as.numeric(RawFeedingVisitsBothSexes$Sex )


{# creation of i simulated dataset (and calculation of i Asim) for each j file

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 

out_Asim_j = list()
out_Asim_i = list()

for (j in 1:length(unique(RawFeedingVisitsBothSexes$DVDRef))){
  
  x <- split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[j]]
  
  # split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[2]] # a normal file
  # split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[1]] # only one male visit
  # split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[13]] # only 2 female visits > screw up the function 'sample'
  # split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[['935']] # no female visits > now removed
  # split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[15]] # only one male and one female visit
  
  x <- x[order(x$TstartFeedVisit),]
  x0 <- x[x$Sex==0,]
  x1 <- x[x$Sex==1,]
  
  
  for (i in 1:100) # to increase up to 1000
  {
    
    x0sim <- x0
    x1sim <- x1
    
    x0sim$Interval <- c(0, sample_vector(x0sim$Interval[-1]))
    x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1],x0sim$TstartFeedVisit[-nrow(x0sim)]+x0sim$Interval[-1])
    
    x1sim$Interval <- c(0, sample_vector(x1sim$Interval[-1]))
    x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])
    
    xsim <- rbind(x0sim,x1sim)
    xsim <- xsim[order(xsim$TstartFeedVisit),]
    
    Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)
    
    out_Asim_i[i] <- Asim
    out_Asim_j[j] <- list(unlist(out_Asim_i))
    
    # clean up
    x0sim <- NULL
    x1sim <- NULL
    Asim <- NULL
  }
  
  # clean up
  x <- NULL
  x0 <- NULL
  x1 <- NULL
  
}

out_Asim <- do.call(rbind, out_Asim_j)

}

head(out_Asim)

{# out A sim summary

out_Asim_df <- data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef), out_Asim)
out_Asim_df <- merge(x=out_Asim_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Asim_df_perDiffVisit1Rate <- split(out_Asim_df,out_Asim_df$DiffVisit1Rate)

x <-out_Asim_df_perDiffVisit1Rate[[31]]
x <-out_Asim_df_perDiffVisit1Rate[[30]] # just one file


out_Asim_df_perDiffVisit1Rate_fun <- function(x) {
  
  x <- x[,-1]
  x <- x[,-ncol(x)]
  v <- unlist(list(x))
  
  return(c(
    mean(v), # Amean
    mean(v) - sd(v)/sqrt(length(v))*1.96, # Alower
    mean(v) + sd(v)/sqrt(length(v))*1.96, # Aupper
    nrow(x), # NbFiles
    sd(v) #StdDev
  ))
}

out_Asim_df_perDiffVisit1Rate_out1 <- lapply(out_Asim_df_perDiffVisit1Rate,out_Asim_df_perDiffVisit1Rate_fun)
out_Asim_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Asim_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Asim_df_perDiffVisit1Rate_out1))

nrow(out_Asim_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Asim_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Asim_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles', 'StdDev')

}

head(out_Asim_df_perDiffVisit1Rate_out2)


{# summary Aobserved when both sexes visit

MY_tblParentalCare_perVisitRateDiff_bothSexes <- group_by(MY_tblParentalCare, DiffVisit1Rate)

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- summarise (MY_tblParentalCare_perVisitRateDiff_bothSexes,
                                                                    Amean = mean(AlternationValue),
                                                                    Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
                                                                    Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
                                                                    NbFiles = n(),
                                                                    StdDev = sd(AlternationValue))

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes,VisitRateDifference= DiffVisit1Rate)

}

as.data.frame(Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes)


{# for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes$Type <- "Observed"
out_Asim_df_perDiffVisit1Rate_out2$Type <- "Expected"


VisitRateDiff_Amean_bis <- as.data.frame(rbind( Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes[1:21,],out_Asim_df_perDiffVisit1Rate_out2[1:21,] ))
VisitRateDiff_Amean_bis$VisitRateDifference <- as.numeric(as.character(VisitRateDiff_Amean_bis$VisitRateDifference))

# T tests for each rate difference
# In the form:
#t.test2(observedmean, expectedmean, obsSD, expSD, obsN, expN)
t.test2(70.84855, 59.63889 , 14.408150, 12.317041 , 138, 138) # Rate diff 0, p<0.001
t.test2(69.14567, 58.77129 , 12.663135, 11.465658 , 254, 254) # Rate diff 1, p<0.001
t.test2(65.93722, 56.45784 , 12.395797, 10.563150 , 266, 266) # Rate diff 2, p<0.001
t.test2(63.38495, 54.86801 , 12.234819, 10.597378 , 186, 186) # Rate diff 3, p<0.001
t.test2(61.55070, 53.23199 , 11.838540, 9.909829 , 142, 142) # Rate diff 4, p<0.001
t.test2(57.78489, 50.41791 , 11.590494, 10.593014 , 139, 139) # Rate diff 5, p<0.001
t.test2(55.89107, 49.97103 , 10.589231, 9.360344 , 112, 112) # Rate diff 6, p<0.001
t.test2(50.40500, 45.73492 , 13.496825, 11.625657 , 80, 80) # Rate diff 7, p=0.02
t.test2(50.74000, 45.35180 , 13.073493, 10.998949 , 75, 75) # Rate diff 8, p=0.007
t.test2(49.57941, 43.59890 , 13.651467, 11.832954 , 68, 68) # Rate diff 9, p=0.007
t.test2(47.15273, 41.99480 , 13.767606, 11.837745 , 55, 55) # Rate diff 10, p=0.038
t.test2(47.40417, 42.42151 , 11.758455, 9.667830 , 48, 48) # Rate diff 11, p=0.026
t.test2(43.86667, 38.37898 , 14.688632, 12.642359 , 45, 45) # Rate diff 12, n.s p=0.06
t.test2(42.72593, 38.68534 , 14.379595, 11.627518 , 27, 27) # Rate diff 13, n.s p=0.26
t.test2(40.98500, 37.90794 , 15.432716, 12.334537 , 20, 20) # Rate diff 14, n.s p=0.49
t.test2(37.18571, 34.07204 , 13.074629, 11.868425 , 14, 14) # Rate diff 15, n.s p=0.52
t.test2(33.63125, 31.10041 , 13.835448, 11.699797 , 16, 16) # Rate diff 16, n.s p=0.58
t.test2(42.53077, 38.74624 , 9.561937, 7.926035 , 13, 13) # Rate diff 17, n.s p=0.28
t.test2(32.65294, 29.85418 , 14.796246, 12.777751 , 17, 17) # Rate diff 18, n.s p=0.55
t.test2(26.07000, 24.59291 , 14.047780, 12.262149 , 10, 10) # Rate diff 19, n.s p=0.81
t.test2(31.73333, 27.83850 , 16.105977, 13.472257 , 9, 9) # Rate diff 20, n.s p=0.59


# Try anova
# First make dataframe of sim and obs raw data
out_Asim_df_perDVDRefandrew <- split(out_Asim_df,out_Asim_df$DVDRef)
out_Asim_df_perDiffVisit1Rate_fun2 <- function(x) {
  
  x <- x[,-1]
  x <- x[,-ncol(x)]
  v <- unlist(list(x))
  
  return(c(
    mean(v) # Amean
  ))
}

out_Asim_df_out1andrew <- lapply(out_Asim_df_perDVDRefandrew,out_Asim_df_perDiffVisit1Rate_fun2)
out_Asim_df_out2andrew <- data.frame(rownames(do.call(rbind,out_Asim_df_out1andrew)),do.call(rbind, out_Asim_df_out1andrew))

nrow(out_Asim_df_out2andrew)	# 1768
rownames(out_Asim_df_out2andrew) <- NULL
colnames(out_Asim_df_out2andrew) <- c('DVDRef','MeanSimAlt')

}

Observed<- select(MY_tblParentalCare, DVDRef, AlternationValue, DiffVisit1Rate)
Expected<- out_Asim_df_out2andrew
DVDRefdata<- select(Observed, DVDRef, DiffVisit1Rate)
DVDRefdata$DVDRef<-as.factor(DVDRefdata$DVDRef)
Expected<- left_join(Expected, DVDRefdata, by = "DVDRef")
Expected<- rename(Expected, AlternationValue = MeanSimAlt)

Observed$Type <- "Observed"
Expected$Type <- "Expected"

Expected$DVDRef<-as.integer(Expected$DVDRef)

anovadata<-bind_rows(Observed, Expected)
anovadata$DiffVisit1Rate<-as.integer(anovadata$DiffVisit1Rate)
anovadata<-filter(anovadata, DiffVisit1Rate<=20)
anovadata$DiffVisit1Rate<-as.factor(anovadata$DiffVisit1Rate)
anovadata$Type<-as.factor(anovadata$Type)

# Does the difference in prov rate affect alternation? ANOVA
# Yes (as expected) F=52.92 df=19,1705, p<0.001
Observed$DiffVisit1Rate<-as.integer(Observed$DiffVisit1Rate)
Observed20<-filter(Observed, DiffVisit1Rate<=20)
Observed20$DiffVisit1Rate<-as.factor(Observed20$DiffVisit1Rate)
anmod<-lm(AlternationValue ~ DiffVisit1Rate, data=Observed20)
anova(anmod)
summary(anmod)

with(anovadata, t.test(AlternationValue[DiffVisit1Rate==1], AlternationValue[DiffVisit1Rate==2]))
#above is way to go by doing Observed1 Expected1 etc
anovadata$Category<-paste(anovadata$Type, anovadata$DiffVisit1Rate)
anovadata$Category<-as.factor(anovadata$Category)

# This for loop splits into each rate category
# Then does a test of observed vs expected
Rate<-NULL
for (i in 0:20) # incr to 20
{
  Rate<-filter(anovadata, DiffVisit1Rate==i)
  Model<- lm(AlternationValue ~ Type, data=Rate)
  print(i)
  Tukey<- glht(Model, linfct=mcp(Type="Tukey"))
  print(summary(Tukey))
}
# Not sure if this is correct way to do (this way up to 11 is significant,
# but in other ways only up to 6 is significant?)
# Such as in this pairwise test (and the really long Tukey (not in script anymore))

# pairwise.t.test(anovadata$AlternationValue, anovadata$Category, p.adjust.method="holm")

# I think separating is fine, only interested in testing Obs vs Exp at each rate difference
# category so the For loop above is ok.
Fig1bis <- ggplot(data=VisitRateDiff_Amean_bis, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point(size=3)+
  geom_line(size=1)+
  annotate("text", x=0:6,y=c(80, 78, 76, 74, 72, 70, 68),label="***", size=6)+
  annotate("text", x=c(8,9),y=c(64, 62),label="**", size=6)+
  annotate("text", x=c(7,10,11),y=c(66, 60, 58),label="*", size=6)+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation (%)")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "Mean Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0, 100)) +
  theme_classic(base_size=20)+
  theme(
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
  

}



Fig1bis

# Effect of visit rate difference on alternation 
# F(1,31)= 172.7, p<0.001
altvisit<-lm(Amean ~ VisitRateDifference, data=Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes)
par(mfrow=c(2,2))
plot(altvisit)

anova(altvisit)
summary(altvisit)

{### comparison both method of randomization

VisitRateDiff_Amean$TypeSim <- c(rep('ExpectedKat',20),rep('ObservedKat',20))
VisitRateDiff_Amean_bis$TypeSim <- c(rep('ObservedMalika',21),rep('ExpectedMalika',21))
VisitRateDiff_Amean_for_comparison <- rbind(VisitRateDiff_Amean,VisitRateDiff_Amean_bis[,-5])
VisitRateDiff_Amean_for_comparison <- VisitRateDiff_Amean_for_comparison[VisitRateDiff_Amean_for_comparison$TypeSim != 'ObservedKat',]

Fig1comparison <- ggplot(data=VisitRateDiff_Amean_for_comparison, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("red", 'orange','grey'), labels=c("95% Expected Kat", "95% Expected Malika","95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$Amean, n = 9)) +  
  theme_classic()
}  

Fig1comparison 

{# add MeanAsim and Adev for each DVD file to MyTable

MyTable <- merge(y=data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef),MeanAsim = rowMeans(out_Asim)), 
                  x= MyTable, by='DVDRef', all.x =TRUE)

MyTable$Adev <-  MyTable$AlternationValue - MyTable$MeanAsim

}



# Does alternation increase with Brood Number?
MyTable<- filter(MyTable, !is.na(RelTimeMins))
MyTable<- filter(MyTable, !is.na(SocialDadID))
MyTable<- filter(MyTable, !is.na(SocialMumID))





modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                (1 | PairID) + (1 | BroodRef), data=MyTable)
summary(modela)

diagnosticsmodela <- fortify(modela)

# Residuals vs fitted plot:
p1modela<- ggplot(diagnosticsmodela, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
p1modela
# Q-Q plot
p2modela<-ggplot(diagnosticsmodela, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
p2modela

# Testing Fixed Effects
# BroodNumber
modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                (1 | PairID) + (1 | BroodRef), data=MyTable)

modelb<- lmer(AlternationValue ~ 1               + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                (1 | PairID) + (1 | BroodRef), data=MyTable)
anova(modela, modelb)
# Chisq = 1.6912, df = 1, p = 0.1934. No effect of BroodNumber on Alternation

# Chick Age
modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
modelc<- lmer(AlternationValue ~ 1 + PairBroodNb +           RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
anova(modela, modelc)
# Chisq = 52.168, df = 1, p < 0.001. Significant effect of Chick Age on Alternation

# Time of Day (relative to Sunrise) 
modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
modeld<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge +                  NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
anova(modela, modeld)
# Chisq = 18.795, df = 1, p < 0.001. Significant effect of time of day on Alternation

# Chick Number
modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
modele<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + 
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
anova(modela, modele)
# Chisq = 5.2238, df = 1, p = 0.022. Significant effect of number of chicks on Alternation

# Visit Rate Difference
modela<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                DiffVisit1Rate + (1 | PairID) + (1 | BroodRef), data=MyTable)
modelf<- lmer(AlternationValue ~ 1 + PairBroodNb + ChickAge + RelTimeMins + NbHatched +
                (1 | PairID) + (1 | BroodRef), data=MyTable)
anova(modela, modelf)
# Chisq = 785.69, df = 1, p < 0.001. Significant effect of visit rate difference on Alternation.


###
# Fitness
##

# Need to average MyTable by brood, to get mean alternation for each brood.
MyTableBroods <- select(MyTable, BroodRef, AlternationValue, AvgMass, AvgTarsus, Nb3, NbHatched,
                        DadAge, MumAge, ParentsAge, SocialMumID, SocialDadID, PairID, BreedingYear,
                        MFVisitRate, Adev, PairBroodNb, NestboxRef
)
MyTableBroods <- MyTableBroods%>%
  group_by(BroodRef)%>%
  mutate(MeanAlternation = mean(AlternationValue))%>%
  mutate(MeanMFVisitRate = mean(MFVisitRate))%>%
  mutate(MeanAdev = mean(Adev))%>%
  select(-AlternationValue)%>%
  select(-MFVisitRate)%>%
  select(-Adev)%>%
  distinct()


# Chick Mass
ggplot(MyTableBroods, aes(x=MeanAlternation, y=AvgMass))+
  geom_point(col= 'steelblue')+
  geom_smooth(method= 'lm')+
  theme_classic()

fitness1<-lm(AvgMass~MeanAlternation, data=MyTableBroods)
par(mfrow=c(2,2))
plot(fitness1)

anova(fitness1) # Alternation has p=0.004
summary(fitness1) # Rsq= 0.009, F= 8.277, df = 1, 885, p = 0.004

# Add Covariates to above

fitness2<-lm(AvgMass~MeanAlternation+AvgTarsus+NbHatched, data=MyTableBroods)
par(mfrow=c(2,2))
plot(fitness2)

anova(fitness2) 
summary(fitness2) 
# This one is better. Rsq = 0.6555, F = 558.2, DF=3,880, p<0.001
# Tarsus length and brood size are significantly related to chick mass (as expected) 
# Alternation is not

# Does alternation promote greater investment?
investplot1<-ggplot(MyTableBroods, aes(x=MeanAdev, y=MeanMFVisitRate))+
  geom_point()+
  geom_smooth(method="lm", colour="black")+
  scale_x_continuous(breaks = c(-20,-10,0,10,20,30,40))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60))+
  xlab("Mean deviation from expected alternation")+
  ylab("Mean total provisioning rate (feeds/hour)")+
  #geom_vline(xintercept=mean(MyTableBroods$MeanAdev), size= 1, linetype= "dashed", colour="indianred")+
  theme_classic(base_size=15)+
  theme(
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
investplot1

invest1<-lmer(MeanMFVisitRate ~ MeanAdev + NbHatched + (1|NestboxRef) + (1|SocialDadID) + (1|SocialMumID) + (1|BreedingYear) + (1|PairID), data=MyTableBroods)
par(mfrow=c(2,2))
plot(invest1)
anova(invest1)
summary(invest1)

invest1.1<- lmer(MeanMFVisitRate ~ MeanAdev + (1|NestboxRef) + (1|SocialDadID) + (1|SocialMumID) + (1|BreedingYear) + (1|PairID), data=MyTableBroods)
anova(invest1, invest1.1)

invest1.2<- lmer(MeanMFVisitRate ~ NbHatched + (1|NestboxRef) + (1|SocialDadID) + (1|SocialMumID) + (1|BreedingYear) + (1|PairID), data=MyTableBroods)
anova(invest1, invest1.2)


mod<- lm(MeanMFVisitRate ~ MeanAdev, data=MyTableBroods)
anova(mod)
summary(mod)
# Try putting MeanAdev into categories?
MyTableBroods<- transform(MyTableBroods, RoundMeanAdev = round(MeanAdev))
hist(MyTableBroods$RoundMeanAdev)
MyTableBroodsSummary<- group_by(MyTableBroods, RoundMeanAdev) %>%
  summarise(meanvisitrate = mean(MeanMFVisitRate),
            sdvisitrate = sd(MeanMFVisitRate),
            meanhatched = mean(NbHatched))
investplot2<-ggplot(MyTableBroodsSummary, aes(x=RoundMeanAdev, y=meanvisitrate))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_vline(xintercept=mean(MyTableBroodsSummary$RoundMeanAdev), size= 1, linetype= "dashed", colour="indianred")+
  theme_classic()
investplot2

grid.arrange(investplot1, investplot2)

invest2<-lm(meanvisitrate ~ RoundMeanAdev + meanhatched, data=MyTableBroodsSummary)
par(mfrow=c(2,2))
plot(invest2)
anova(invest2)
summary(invest2)

# MeanADev vs MeanMFVisitRate looks like a curve with "optimal" deviation at around +6.
# Can the deviation from expected better explain fitness? Answer - no!!
fitness3<-lm(AvgMass~MeanAdev+AvgTarsus+NbHatched, data=MyTableBroods)
par(mfrow=c(2,2))
plot(fitness3)

anova(fitness3) 
summary(fitness3) 

#########
# Survival to Next Year
####

# Get the Year Dad and Mum last seen alive
MY_tblBroods$DadLastAliveYear <- as.character(MY_tblBroods$LastLiveRecordSocialDad)
MY_tblBroods$DadLastAliveYear <- as.Date(MY_tblBroods$DadLastAliveYear, "%d.%m.%Y")
MY_tblBroods$DadLastAliveYear <- as.numeric(format(MY_tblBroods$DadLastAliveYear, "%Y"))

MY_tblBroods$MumLastAliveYear <- as.character(MY_tblBroods$LastLiveRecordSocialMum)
MY_tblBroods$MumLastAliveYear <- as.Date(MY_tblBroods$MumLastAliveYear, "%d.%m.%Y")
MY_tblBroods$MumLastAliveYear <- as.numeric(format(MY_tblBroods$MumLastAliveYear, "%Y"))

# Get survival to next year (Binomial response, 1= survive, 0= not)
MY_tblBroods <- mutate(MY_tblBroods, DadSurvivalToNextYear = ifelse(DadLastAliveYear > BreedingYear, 1, 0))
MY_tblBroods <- mutate(MY_tblBroods, MumSurvivalToNextYear = ifelse(MumLastAliveYear > BreedingYear, 1, 0))

ParentalSurvival<- select(MY_tblBroods, BroodRef, DadSurvivalToNextYear, MumSurvivalToNextYear)
MyTableBroods<- left_join(MyTableBroods, ParentalSurvival, by = "BroodRef")
MyTableBroods$SocialDadID<-as.integer(MyTableBroods$SocialDadID)
MyTableBroods$SocialMumID<-as.integer(MyTableBroods$SocialMumID)

# Model survival
# Cannot include 2015 breeding year as we do not know if they survived yet
MyTableBroodsNo2015<- filter(MyTableBroods, BreedingYear!=2015)

MyTableBroodsNo2015$SocialDadID<-as.factor(MyTableBroodsNo2015$SocialDadID)
malesurvivalmodel <- glmer(DadSurvivalToNextYear ~ 1 + MeanAlternation + DadAge + (1|SocialDadID) + (1|BreedingYear), MyTableBroodsNo2015, binomial)
anova(malesurvivalmodel)
summary(malesurvivalmodel)

femalesurvivalmodel <- glmer(MumSurvivalToNextYear ~ 1 + MeanAlternation + MumAge + (1|BreedingYear), MyTableBroodsNo2015, binomial)
anova(femalesurvivalmodel)
summary(femalesurvivalmodel)

## Proportion of successfully reared chicks? (Chicks fledged / Chicks hatched)
# Need to revisit this (as of 11052016) convergence???
chicksuccessmodel<- glmer(cbind(Nb3, NbHatched) ~ MeanAlternation + MeanMFVisitRate + (1|PairID) + (1|BroodRef), MyTableBroods, binomial)
anova(chicksuccessmodel)
summary(chicksuccessmodel)

##
ggplot(MyTableBroods, aes(x=DadAge, y=MeanAlternation))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

dadagemod <- lm(MeanAlternation ~ DadAge + (1|SocialDadID), data=MyTableBroods)
par(mfrow=c(2,2))
plot(dadagemod)
anova(dadagemod)
summary(dadagemod)

# Predictor model similar to Malika style
# Chick age needs to be in categories because of field protocol

MyTable$ChickAgeCat[MyTable$ChickAge <10 ] <- 'Age06'
MyTable$ChickAgeCat[MyTable$ChickAge >=10 ] <- 'Age10'
MyTable$ChickAgeCat<-as.factor(MyTable$ChickAgeCat)
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
summary(predictormodel)

diagnosticspredictormodel <- fortify(predictormodel)

# Residuals vs fitted plot:
predictplot<- ggplot(diagnosticspredictormodel, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
predictplot
# Q-Q plot
predictplot2<-ggplot(diagnosticspredictormodel, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
predictplot2

# Is Age significant? Yes p<0.001
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
predictormodel2<- lmer(AlternationValue ~  PairBroodNb + ChickAgeCat + RelTimeMins + 
                         NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                         (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel, predictormodel2)

# Is brood number sig? No, p=0.08
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
predictormodel3<- lmer(AlternationValue ~ ParentsAge +  ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel, predictormodel3)

# Is chick age sig? Yes p<0.001
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
predictormodel4<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel, predictormodel4)

# Is time of day sig? Yes p<0.001
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
predictormodel5<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat +  
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel, predictormodel5)

# Is number of chicks sig? Yes p=0.012
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)

predictormodel6<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel,predictormodel6)

# Is the difference in prov rate sig? Yes p<0.001
predictormodel<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + DiffVisit1Rate + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
predictormodel7<- lmer(AlternationValue ~ ParentsAge + PairBroodNb + ChickAgeCat + RelTimeMins + 
                        NbHatched + (1 | SocialDadID) + (1 | SocialMumID) +
                        (1 | PairID) + (1 | BroodRef) + (1 | BreedingYear), data=MyTable)
anova(predictormodel,predictormodel7)

# Mean and SE of predictors
mean(MyTable$ParentsAge)
sd(MyTable$ParentsAge)/sqrt(length(MyTable$ParentsAge))*1.96
mean(MyTable$ChickAge)
sd(MyTable$ChickAge)/sqrt(length(MyTable$ChickAge))*1.96
mean(MyTable$RelTimeMins)
sd(MyTable$RelTimeMins)/sqrt(length(MyTable$RelTimeMins))*1.96
mean(MyTable$NbHatched)
sd(MyTable$NbHatched)/sqrt(length(MyTable$NbHatched))*1.96
mean(MyTable$PairBroodNb)
sd(MyTable$PairBroodNb)/sqrt(length(MyTable$PairBroodNb))*1.96
mean(MyTable$DiffVisit1Rate)
sd(MyTable$DiffVisit1Rate)/sqrt(length(MyTable$DiffVisit1Rate))*1.96

####
# Extra bits
####

# Calculate mortality?

# Loop which prints:
# The year
# Number of females who bred that year
# Number of males who bred that year
# Number of females who survived to the following year
# Number of males who survived to the following year

Broods<- NULL
Mum<- NULL
Dad<- NULL
for (i in 2004:2015) # incr to 2015
{
  Broods<-filter(MY_tblBroods, BreedingYear==i)
  print(i)
  print(length(unique(Broods$SocialMumID))) 
  print(length(unique(Broods$SocialDadID)))
  Broods<- select(Broods, SocialMumID, SocialDadID, MumSurvivalToNextYear, DadSurvivalToNextYear)
  Mum<-select(Broods, SocialMumID, MumSurvivalToNextYear)
  Dad<-select(Broods, SocialDadID, DadSurvivalToNextYear)
  Mum<-rename(Mum, ID = SocialMumID)
  Mum<-rename(Mum, Survival = MumSurvivalToNextYear)
  Dad<-rename(Dad, ID = SocialDadID)
  Dad<-rename(Dad, Survival = DadSurvivalToNextYear)
  Broods<-bind_rows(Dad, Mum)
  Broods<-distinct(Broods)
  print(table(Broods$Survival))
}
# This is the output ( need to automate from the loop but not sure how )
survival<- data.frame(Survival = c(0.858,0.468,0.449,0.274,0.632,0.614,0.733,0.304,0.873,0.747,0.352,0.287),
                      Year = 2004:2015)

ggplot(survival, aes(x=Year, y=Survival))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=0.54925, size= 1, linetype= "dashed", colour="indianred")+
  ylim(0,1)+
  theme_classic()

####
# Deviance from mass/tarsus relationship
ggplot(MyTableBroods, aes(x=AvgTarsus, y=AvgMass))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

devmassmod<-lm(AvgMass ~ AvgTarsus, MyTableBroods)
par(mfrow=c(2,2))
plot(devmassmod)
summary(devmassmod)
residuals(devmassmod)

MyTableBroodsForMass<- filter(MyTableBroods, !is.na(AvgMass))
MyTableBroodsForMass<- filter(MyTableBroods, !is.na(AvgTarsus))

MyTableBroodsForMass$DevMass<-residuals(devmassmod)
fitnessdevmass<-lm(DevMass~MeanAlternation+AvgTarsus+NbHatched, data=MyTableBroodsForMass)
plot(fitnessdevmass)
anova(fitnessdevmass)
summary(fitnessdevmass)


vrmod<- lmer((Nb3/NbHatched)~MeanMFVisitRate+MeanAlternation+ParentsAge+PairBroodNb+(1|SocialMumID) + (1|SocialDadID) + (1|BreedingYear), data=MyTableBroods)
par(mfrow=c(2,2))
plot(vrmod)
anova(vrmod)
summary(vrmod)
ggplot(MyTableBroods, aes(x=MeanMFVisitRate, y=(Nb3/NbHatched)))+
  geom_point()+
  theme_classic()


###
# Add MumID and DadID to Raw Interfeeds
MaleIDs<- select(MyTable, DVDRef, SocialDadID)
FemaleIDs<- select(MyTable, DVDRef, SocialMumID)
MRawInterfeeds<- left_join(MRawInterfeeds, MaleIDs, by='DVDRef')
FRawInterfeeds<- left_join(FRawInterfeeds, FemaleIDs, by='DVDRef')
MRawInterfeeds<- rename(MRawInterfeeds, ID = SocialDadID)
FRawInterfeeds<- rename(FRawInterfeeds, ID = SocialMumID)
MRawInterfeeds<- select(MRawInterfeeds, -MVisit1RateH)
FRawInterfeeds<- select(FRawInterfeeds, -FVisit1RateH)




MRawInterfeeds<- group_by(MRawInterfeeds, DVDRef)

MRawInterfeeds<-mutate(MRawInterfeeds, diff = (Interval - dplyr::lag(Interval, default=first(Interval))))
# If difference is negative (interval getting smaller) then 0
#make diff=0 NA
MRawInterfeeds$diff[MRawInterfeeds$diff==0]<-NA
MRawInterfeeds<-mutate(MRawInterfeeds, Difference = as.numeric(ifelse((diff<0), 0, 1)))

# Find average Difference to give P value for brood

MInterfeedSummary<-
  MRawInterfeeds%>%
  group_by(DVDRef)%>%
  filter(!is.na(Difference))%>%
  summarise(
    MaleP = mean(Difference))

mean(MInterfeedSummary$MaleP)

# Females
FRawInterfeeds<- group_by(FRawInterfeeds, DVDRef)

FRawInterfeeds<-mutate(FRawInterfeeds, diff = (Interval - dplyr::lag(Interval, default=first(Interval))))
# If difference is negative (interval getting smaller) then 0
#make diff=0 NA
FRawInterfeeds$diff[FRawInterfeeds$diff==0]<-NA
FRawInterfeeds<-mutate(FRawInterfeeds, Difference = as.numeric(ifelse((diff<0), 0, 1)))

# Find average Difference to give P value for brood

FInterfeedSummary<-
  FRawInterfeeds%>%
  group_by(DVDRef)%>%
  filter(!is.na(Difference))%>%
  summarise(
    FemaleP = mean(Difference))

mean(FInterfeedSummary$FemaleP)

InterfeedSummary<- left_join(MInterfeedSummary, FInterfeedSummary, by="DVDRef")

ggplot(InterfeedSummary, aes(x=FemaleP, y=MaleP))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  theme_classic()

InterfeedSummary<-mutate(InterfeedSummary, MalePDash = ifelse(MaleP<=0.5, MaleP, (1-MaleP)))
InterfeedSummary<-mutate(InterfeedSummary, FemalePDash = ifelse(FemaleP<=0.5, FemaleP, (1-FemaleP)))
par(mfrow=c(1,2))
hist(InterfeedSummary$MalePDash)
hist(InterfeedSummary$FemalePDash)


#####
adevmod<-lm(FVisit1RateH~NbHatched, data=MY_tblParentalCare)
par(mfrow=c(2,2))
plot(adevmod)
anova(adevmod)
summary(adevmod)


# Survival needs to look at mean alternation for INDIVIDUAL in a YEAR
MyTableBroodsNo2015$SocialDadID<-as.integer(MyTableBroodsNo2015$SocialDadID)
MyTableBroodsNo2015<-unite(MyTableBroodsNo2015, YearsMum, BreedingYear, SocialMumID, sep="", remove=FALSE)
MyTableBroodsNo2015<-unite(MyTableBroodsNo2015, YearsDad, BreedingYear, SocialDadID, sep="", remove=FALSE)
Dads<-select(MyTableBroodsNo2015, YearsDad, MeanAlternation, DadAge, SocialDadID, BreedingYear, DadSurvivalToNextYear)
Mums<-select(MyTableBroodsNo2015, YearsMum, MeanAlternation, MumAge, SocialMumID, BreedingYear, MumSurvivalToNextYear)

DadsByYear<- summarise(group_by(Dads, YearsDad),
                       MeanAlternation = mean(MeanAlternation),
                       DadAge = mean(DadAge),
                       SocialDadID = mean(SocialDadID),
                       BreedingYear = mean(BreedingYear),
                       Survival = mean(DadSurvivalToNextYear))

MumsByYear<- summarise(group_by(Mums, YearsMum),
                       MeanAlternation = mean(MeanAlternation),
                       MumAge = mean(MumAge),
                       SocialMumID = mean(SocialMumID),
                       BreedingYear = mean(BreedingYear),
                       Survival = mean(MumSurvivalToNextYear))

malesurvivalmodel2 <- glmer(Survival ~ 1 + MeanAlternation + DadAge + (1|SocialDadID) + (1|BreedingYear), DadsByYear, binomial)
anova(malesurvivalmodel2)
summary(malesurvivalmodel2)

femalesurvivalmodel2 <- glmer(Survival ~ 1 + MeanAlternation + MumAge + (1|SocialMumID) + (1|BreedingYear), MumsByYear, binomial)
anova(femalesurvivalmodel2)
summary(femalesurvivalmodel2)
