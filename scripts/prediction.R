#Install packages
#Import packages
library(dplyr)


#Set work directory

setwd("/Users/egeoguzman/Desktop/NCI/MLDM")

#Read Data

full_test_dataset <- read.csv(file='processed_data/full_test_dataset.csv', na.strings = '..',stringsAsFactors = FALSE)
Teams <- read.csv(file='data/MTeams.csv', na.strings = '..',stringsAsFactors = FALSE)
names(full_test_dataset)

matchups_and_stats <- data.frame(matrix(ncol = 24, nrow = 0))
x <- c("T1ID","T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2ID","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO")
colnames(matchups_and_stats) <- x
matchups_and_stats

for (team_1 in full_test_dataset$TeamID){
  team_1_stats = full_test_dataset[full_test_dataset[, "TeamID"] == team_1,]
  team_1_stats_cols = team_1_stats[, c("TeamID","FGP","Three3Pper","FTR","OR","DR","TO","Stl","Blk","PF","Ast","ELO")]
  names(team_1_stats_cols) =  c("T1ID","T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO")
  for (team_2 in full_test_dataset$TeamID){
    if (team_1 < team_2){
      team_2_stats <- full_test_dataset[full_test_dataset[, "TeamID"] == team_2,]
      team_2_stats_cols <- team_2_stats[, c("TeamID","FGP","Three3Pper","FTR","OR","DR","TO","Stl","Blk","PF","Ast","ELO")]
      names(team_2_stats_cols) =  c("T2ID","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO")
      
      matchup <- cbind(team_1_stats_cols,team_2_stats_cols)
      matchups_and_stats[nrow(matchups_and_stats) + 1, ] <- matchup
    }
  }
}
dim(matchups_and_stats)

#### LOAD THE MODEL #####
lr_model <- readRDS("models/lr_model.rds")
results <- predict(lr_model, matchups_and_stats[,-c(1,13)], type = "response")
results

matchups_and_stats$probs <- results
matchups_and_stats$won <- ifelse (results > 0.5,1,0)
dim(matchups_and_stats)

### LESS READBLE RESULTS ####
final.results <- data.frame(matchups_and_stats$T1ID, matchups_and_stats$T2ID, matchups_and_stats$probs)
final.results

### MORE READBLE RESULTS ####

finalresults.2021 <- final.results %>%
  left_join(select(Teams, matchups_and_stats.T1ID = TeamID, Team1Name = TeamName), by = c("matchups_and_stats.T1ID")) %>% 
  left_join(select(Teams, matchups_and_stats.T2ID = TeamID, Team2Name = TeamName), by = c("matchups_and_stats.T2ID")) %>% 
  transmute(Team1Name,Team2Name)

finalresults.2021$Winner <- matchups_and_stats$won

final.results %>% write.csv('bracket/less_readable_matchups.csv')
finalresults.2021 %>% write.csv('bracket/more_readable_matchups.csv')