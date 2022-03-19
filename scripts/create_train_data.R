#Import packages
library(dplyr)


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
KenPomRanks <- read.csv(file='data/NCAA2021_Kenpom.csv', na.strings = '..',stringsAsFactors = FALSE)

head(MRegularSeasonDetailedResults)
head(MNCAATourneyDetailedResults)

#Feature Engineering 1 ------- Double the data by swapping winner and looser for REGULAR SEASON

r1 = MRegularSeasonDetailedResults[, c("Season","DayNum","WTeamID","WScore","LTeamID","LScore","WLoc","NumOT","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF")]
#r2 = MRegularSeasonDetailedResults[, c("Season","DayNum","LTeamID","LScore","WTeamID","WScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF")]
#names(r1) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
#names(r2) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
#regular_season = rbind(r1, r2)


#Feature Engineering 1.1 ------- Double the data by swapping winner and looser for TOURNEY
t1 = MNCAATourneyDetailedResults[, c("Season","DayNum","WTeamID","WScore","LTeamID","LScore","WLoc","NumOT","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF")]
#t2 = MNCAATourneyDetailedResults[, c("Season","DayNum","LTeamID","LScore","WTeamID","WScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF")]
#names(t1) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
#names(t2) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
#tourney = rbind(t1, t2)

#Combine regular and tourney results
reg_tourney <- rbind(r1, t1)

half_1 <- reg_tourney[1:round(nrow(reg_tourney)/2), ]
half_2 <- reg_tourney[(round(nrow(reg_tourney)/2)+1):nrow(reg_tourney), ]

half_2 = half_2[, c("Season","DayNum","LTeamID","LScore","WTeamID","WScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF")]

names(half_1) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
names(half_2) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")

names(half_1)
names(half_2)

combined_stats <- rbind(half_1, half_2)

#Feature Engineering 2 ------- Generate New Stats by Combining Others
combined_stats <- combined_stats %>%
  mutate(T1FGP = T1FGM / T1FGA *100,
         T2FGP = T2FGM / T2FGA *100,
         T1Poss = T1FGA + (T1FTA * 0.475) + T1TO - T1OR,
         T2Poss = T2FGA + (T2FTA * 0.475) + T2TO - T2OR,
         T1efgper = ((T1FGM - T1FGM3) + 1.5 * T1FGM3)/ T1FGA,
         T2efgper = ((T2FGM - T2FGM3) + 1.5 * T2FGM3)/ T2FGA,
         T13Pper = T1FGM3 / T1FGA3 * 100,
         T23Pper = T2FGM3 / T2FGA3 * 100,
         T1OffRat = (T1Score / (T1Poss + T2Poss)) * 100,
         T2OffRat = (T2Score / (T1Poss + T2Poss)) * 100,
         T1DeffRat = (T2Score / (T1Poss + T2Poss)) * 100,
         T2DeffRat = (T1Score / (T1Poss + T2Poss)) * 100,
         T1SOS = T1OffRat / T2OffRat,
         T2SOS = T2OffRat / T1OffRat,
         T1TOPoss = T1TO / T1Poss,
         T2TOPoss = T2TO / T2Poss,
         T1ORper = T1OR / (T1OR + T2DR),
         T2ORper = T2OR / (T2OR + T1DR),
         T1FTR = T1FTM / T1FTA * 100,
         T2FTR = T2FTM / T2FTA * 100)
         
head(combined_stats)
names(combined_stats)

#Feature Engineering 3 ------- ADD POM'S ELO RANKS

pom_elo <- filter(MMasseyOrdinals, Season==2003, SystemName == "POM")
pom_elo2003 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2004, SystemName == "POM")
pom_elo2004 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2005, SystemName == "POM")
pom_elo2005 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2006, SystemName == "POM")
pom_elo2006 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2007, SystemName == "POM")
pom_elo2007 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2008, SystemName == "POM")
pom_elo2008 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2009, SystemName == "POM")
pom_elo2009 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2010, SystemName == "POM")
pom_elo2010 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2011, SystemName == "POM")
pom_elo2011 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2012, SystemName == "POM")
pom_elo2012 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2013, SystemName == "POM")
pom_elo2013 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2014, SystemName == "POM")
pom_elo2014 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2015, SystemName == "POM")
pom_elo2015 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2016, SystemName == "POM")
pom_elo2016 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2017, SystemName == "POM")
pom_elo2017 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2019, SystemName == "POM")
pom_elo2018 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2019, SystemName == "POM")
pom_elo2019 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)
pom_elo <- filter(MMasseyOrdinals, Season==2020, SystemName == "POM")
pom_elo2020 <- pom_elo %>% 
  distinct(TeamID, .keep_all = TRUE)

pom_combined = rbind(pom_elo2003, pom_elo2004,pom_elo2005,pom_elo2006,pom_elo2007,pom_elo2008,pom_elo2009,pom_elo2010,pom_elo2011,pom_elo2012,pom_elo2013,pom_elo2014,pom_elo2015,pom_elo2016,pom_elo2017,pom_elo2018,pom_elo2019,pom_elo2020)

combined_stats <- combined_stats %>%
  left_join(select(pom_combined, Season, T1TeamID = TeamID, T1ELO = OrdinalRank), by = c("Season", "T1TeamID")) %>% 
  left_join(select(pom_combined, Season, T2TeamID = TeamID, T2ELO = OrdinalRank), by = c("Season", "T2TeamID")) %>% 
  mutate(T1ELO,T2ELO)

names(combined_stats)
tail(combined_stats)
combined_stats <- combined_stats %>% distinct()

#Feature Engineering 4 ------- SEED DIFF"

pom_seed <- filter(KenPomRanks)
pom_seed
pom_seed <- pom_seed[,c("Season", "TeamID", "Seed")]
pom_seed$Seed = as.numeric(substring(pom_seed$Seed,2,3))
pom_seed$Seed
combined_stats <- combined_stats %>%
  left_join(select(pom_seed, Season, T1TeamID = TeamID, T1SEED = Seed), by = c("Season", "T1TeamID")) %>% 
  left_join(select(pom_seed, Season, T2TeamID = TeamID, T2SEED = Seed), by = c("Season", "T2TeamID")) %>% 
  mutate(SEEDDIFF = T1SEED-T2SEED)

drops <- c("T1SEED","T2SEED")
combined_stats <- combined_stats[ , !(names(combined_stats) %in% drops)]
combined_stats

#Feature Engineering 5 ------- Calculate the Score diff and create new variable "IS T1 WINNER OR LOOSER?"
combined_stats <- transform(combined_stats,T1WON=ifelse(T1Score>T2Score,1,0))
combined_stats <- combined_stats %>% distinct()
head(combined_stats)
gc()


##FINAL FULL DATASET
#combined_stats <- combined_stats[ , -which(names(combined_stats) %in% c("Season","DayNum", "T1Loc", "T1TeamID", "T2TeamID"))]
names(combined_stats)
head(combined_stats)
nrow(combined_stats)
# SAVE FULL DATA
combined_stats %>% write.csv('processed_data/full_dataset.csv')
