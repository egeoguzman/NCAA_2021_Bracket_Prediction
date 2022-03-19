#Install Packages

install.packages("corrplot")
install.packages("skimr")

#Import Packages

library(corrplot)
library(dplyr)
library(skimr)

#Set work directory

setwd("/Users/egeoguzman/Desktop/NCI/MLDM")

#Read Data

MNCAATourneyDetailedResults <- read.csv(file='data/MNCAATourneyDetailedResults.csv', na.strings = '..',stringsAsFactors = FALSE)
MRegularSeasonDetailedResults <- read.csv(file='data/MRegularSeasonDetailedResults.csv', na.strings = '..',stringsAsFactors = FALSE)
MNCAATourneyCompactResults <- read.csv(file='data/MNCAATourneyCompactResults.csv', na.strings = '..',stringsAsFactors = FALSE)
MRegularSeasonCompactResults <- read.csv(file='data/MRegularSeasonCompactResults.csv', na.strings = '..',stringsAsFactors = FALSE)
MNCAATourneySeeds <- read.csv(file='data/MNCAATourneySeeds.csv', na.strings = '..',stringsAsFactors = FALSE)
MMasseyOrdinals <- read.csv(file='data/MMasseyOrdinals.csv', na.strings = '..',stringsAsFactors = FALSE)
MTeamConferences <- read.csv(file='data/MTeamConferences.csv', na.strings = '..',stringsAsFactors = FALSE)
ELORanks <- read.csv(file='data/elo_rank.csv', na.strings = '..',stringsAsFactors = FALSE)
Teams <- read.csv(file='data/MTeams.csv', na.strings = '..',stringsAsFactors = FALSE)
Conferences <- read.csv(file='data/Conferences.csv', na.strings = '..',stringsAsFactors = FALSE)
#full_dataset <- read.csv(file='/Users/egeoguzman/Desktop/NCI/MLDM/processed_data/full_dataset.csv', na.strings = '..',stringsAsFactors = FALSE)

#Descriptive Statistics of Historical Matches

summary(MNCAATourneyDetailedResults)
summary(MRegularSeasonDetailedResults)

#Some Samples from the Datasets

glimpse(MNCAATourneyDetailedResults)
glimpse(MRegularSeasonDetailedResults)

#Dimensions of Datasets

dim(MNCAATourneyDetailedResults)
dim(MRegularSeasonDetailedResults)
dim(MNCAATourneyCompactResults)
dim(MRegularSeasonCompactResults)
dim(MNCAATourneySeeds)
dim(MMasseyOrdinals)
dim(MTeamConferences)
dim(ELORanks)
dim(Teams)
dim(Conferences)

#Missing Value Detection for Historical Matches

sapply(MNCAATourneyDetailedResults, function(x) sum(is.na(x))) # There are no missing values.
sapply(MRegularSeasonDetailedResults, function(x) sum(is.na(x))) # There are no missing values.

#Detailed Descriptives

skim(MNCAATourneyDetailedResults)
skim(MRegularSeasonDetailedResults)

#Correlation Plot

corr = cor(MNCAATourneyDetailedResults[,-c(1,2,3,5,7,8)])
corrplot(corr, method = 'circle', mar=c(3,3,5,3))

corr = cor(full_dataset[,c(36:59)], use="pairwise.complete.obs")
corrplot(corr, method = 'circle', mar=c(3,3,5,3))

corr = cor(full_dataset[,], use="pairwise.complete.obs")
corrplot(corr, method = 'circle', mar=c(3,3,5,3))
