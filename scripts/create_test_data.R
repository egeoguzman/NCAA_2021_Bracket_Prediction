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
KenPomRanks <- read.csv(file='data/NCAA2021_Kenpom.csv', na.strings = '..')
full_dataset <- read.csv(file='processed_data/full_dataset.csv', na.strings = '..',stringsAsFactors = FALSE)

#GET 2020 REGULAR SEASON TEAM AVERAGE STATS

reg_2020 <- filter(MRegularSeasonDetailedResults, Season==2020)
names(reg_2020)
names(reg_2020) = c("Season","DayNum","T1TeamID","T1Score","T2TeamID","T2Score","T1Loc","NumOT","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF")
reg_2020 <- reg_2020 %>%
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

pom_elo <- filter(KenPomRanks, Season==2021)
pom_elo
pom_elo <- pom_elo[,c("Season", "TeamID", "rank")]
reg_2020 <- reg_2020 %>%
  left_join(select(pom_elo, Season, T1TeamID = TeamID, T1ELO = rank), by = c("Season", "T1TeamID")) %>% 
  left_join(select(pom_elo, Season, T2TeamID = TeamID, T2ELO = rank), by = c("Season", "T2TeamID")) %>% 
  mutate(T1ELO,T2ELO)
names(reg_2020)
#We know which college goes to March Madness with Seeds Dataset.
pom_rank <- filter(KenPomRanks, Season==2021)
pom_rank <- pom_rank[,c("Season", "TeamID", "Seed")]
MM2021TeamList <- pom_rank["TeamID"]
MM2021TeamList
#Calculate average stats of each team for 2020 Regular Season
T1_stats = reg_2020[, c("T1TeamID","T1Score","T1FGM","T1FGA","T1FGM3","T1FGA3","T1FTM","T1FTA","T1OR","T1DR","T1Ast","T1TO","T1Stl","T1Blk","T1PF","T1FGP","T1Poss","T1efgper","T13Pper","T1OffRat","T1DeffRat","T1SOS","T1TOPoss","T1ORper","T1FTR")]
T2_stats = reg_2020[, c("T2TeamID","T2Score","T2FGM","T2FGA","T2FGM3","T2FGA3","T2FTM","T2FTA","T2OR","T2DR","T2Ast","T2TO","T2Stl","T2Blk","T2PF","T2FGP","T2Poss","T2efgper","T23Pper","T2OffRat","T2DeffRat","T2SOS","T2TOPoss","T2ORper","T2FTR")]

avg_stats_T1 <- aggregate(T1_stats[, 2:25], list(T1_stats$T1TeamID), mean)
avg_stats_T2 <- aggregate(T2_stats[, 2:25], list(T2_stats$T2TeamID), mean)


names(avg_stats_T1) = c("TeamID","Score", "FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF","FGP","Poss","efgper","Three3Pper","OffRat","DeffRat","SOS","TOPoss","ORper","FTR")
names(avg_stats_T2) = c("TeamID","Score", "FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF","FGP","Poss","efgper","Three3Pper","OffRat","DeffRat","SOS","TOPoss","ORper","FTR")

combined_avg_stats <- rbind(avg_stats_T1,avg_stats_T2)
names(combined_avg_stats)

final_avg_stats <- aggregate(combined_avg_stats[, 2:25], list(combined_avg_stats$TeamID), mean)
final_avg_stats

pom_elo <- filter(KenPomRanks, Season==2021)
pom_elo <- pom_elo[,c("Season", "TeamID", "rank")]
final_avg_stats_with_elo <- final_avg_stats %>%
  left_join(select(pom_elo, Group.1 = TeamID, ELO = rank), by = c("Group.1"))

pom_seed <- filter(KenPomRanks,Season==2021)
pom_seed
pom_seed <- pom_seed[,c("Season", "TeamID", "Seed")]
pom_seed$Seed = as.numeric(substring(pom_seed$Seed,2,3))
pom_seed$Seed
final_avg_stats_with_elo <- final_avg_stats_with_elo %>%
  left_join(select(pom_seed, Group.1 = TeamID, Seed = Seed), by = c("Group.1"))

names(final_avg_stats_with_elo_last)
sum(is.na(final_avg_stats_with_elo$ELO))
nrow(final_avg_stats_with_elo) - sum(is.na(final_avg_stats_with_elo$ELO))
final_avg_stats_with_elo <- final_avg_stats_with_elo[!is.na(final_avg_stats_with_elo$ELO),]
names(final_avg_stats_with_elo)
nrow(final_avg_stats_with_elo)
head(final_avg_stats_with_elo)

colnames(final_avg_stats_with_elo)[1] <- "TeamID"

final_avg_stats_with_elo %>% write.csv('processed_data/full_test_dataset.csv')
