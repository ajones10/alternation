# 22/03/2016
# Mixed Model attempt
# Predictors of Alternation

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

# Similar query to before, but this time includes more things, such as number of nestlings, time, date,
# which can be used as factors in a model.
# This code reads in the data file AJ_BroodsPerPairUpdated.csv
BroodsPerPairUpdated <- read.csv("C:/Users/Andrew Jones/SkyDrive/Documents/Git Analysis/Project testing/AJ_BroodsPerPairUpdated.csv")

# This merges the alternation summary data with the BroodsPerPairUpdated query from the database

Merged<-merge(Summarydata, BroodsPerPairUpdated, "Filename")