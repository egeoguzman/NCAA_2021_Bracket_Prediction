#Install Packages
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
#Import Packages
library(ggplot2)
library(magrittr)
library(dplyr)
library(gridExtra)
library(scales)
library("stringr")

#Set work directory
setwd("/Users/egeoguzman/Desktop/NCI/MLDM")
#Read Data
MNCAATourneyDetailedResults <- read.csv(file='data/MNCAATourneyDetailedResults.csv', na.strings = '..')
MRegularSeasonDetailedResults <- read.csv(file='data/MRegularSeasonDetailedResults.csv', na.strings = '..')
MNCAATourneyCompactResults <- read.csv(file='data/MNCAATourneyCompactResults.csv', na.strings = '..')
MRegularSeasonCompactResults <- read.csv(file='data/MRegularSeasonCompactResults.csv', na.strings = '..')
MNCAATourneySeeds <- read.csv(file='data/MNCAATourneySeeds.csv', na.strings = '..')
MMasseyOrdinals <- read.csv(file='data/MMasseyOrdinals.csv', na.strings = '..')
MTeamConferences <- read.csv(file='data/MTeamConferences.csv', na.strings = '..')
ELORanks <- read.csv(file='data/elo_rank.csv', na.strings = '..')
Teams <- read.csv(file='data/MTeams.csv', na.strings = '..')
Conferences <- read.csv(file='data/Conferences.csv', na.strings = '..')

#Plot Most Winner Colleges ---BEGIN---

MNCAATourneyCompactResults <- MNCAATourneyCompactResults %>%
  left_join(Teams, by = c("WTeamID" = "TeamID")) %>%
  left_join(Teams, by = c("LTeamID" = "TeamID"))

MNCAATourneyCompactResults <- MNCAATourneyCompactResults %>%
  rename(WTeamName = TeamName.x,
         LTeamName = TeamName.y)

MNCAATourneyCompactResults$season_day <- paste(MNCAATourneyCompactResults$Season, MNCAATourneyCompactResults$DayNum, sep = "_")

MNCAATourneyCompactResults <- MNCAATourneyCompactResults %>%
  mutate(TourneyRound = ifelse(DayNum %in% c(136, 137), "First Round", ifelse(DayNum %in% c(138, 139), "Second Round", ifelse(DayNum %in% c(143, 144), "Sweet 16", ifelse(DayNum %in% c(145, 146), "Elite 8", ifelse(DayNum == 152, "Final Four", "Championship Game")))))) %>%
  mutate(TourneyRound = factor(TourneyRound, levels = c("First Round", "Second Round", "Sweet 16", "Elite 8", "Final Four", "Championship Game")))

ncaa_champs <- MNCAATourneyCompactResults %>%
  group_by(Season) %>%
  summarise(max_days = max(DayNum)) %>%
  mutate(season_day = paste(Season, max_days, sep = "_")) %>%
  left_join(MNCAATourneyCompactResults, by = "season_day") %>% ungroup() %>%
  select(-Season.y) %>%
  rename(Season = Season.x)

win_plot <- ncaa_champs %>%
  group_by(WTeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(WTeamName,n), y=n)) +
  geom_bar(stat = "identity", fill = "#FF6666", color = "grey") +
  labs(title = "Most Winners and Runner-Ups since 1985", subtitle = "Most Tourney Wins") +
  scale_y_continuous(labels = c("0", "1", "2", "3", "4", "5 titles")) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


lose_plot <- ncaa_champs %>%
  group_by(LTeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(LTeamName, n), y=n)) +
  geom_bar(stat = "identity", fill = "#00FFFF", colour = "grey") +
  labs(title = "", subtitle = "Most Tourney Runner-Ups") +
  scale_y_continuous(limits = c(0,5), labels = c("0", "1", "2", "3", "4", "seconds")) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

grid.arrange(win_plot, lose_plot, ncol = 2)

#Plot Most Winner Colleges ---END---

#Plot Conferance Comparison ---BEGIN---

ncaa_champs %>%
  select(Season, TeamID = WTeamID) %>%
  left_join(MTeamConferences, by = c("Season", "TeamID")) %>%
  left_join(Conferences, by = "ConfAbbrev") %>%
  count(Description) %>%
  ggplot(aes(x= reorder(Description, n), y= n)) +
  geom_col(fill = "#FF6666", color = "grey") +
  geom_text(aes(label = n), hjust = 1, size = 3, color = "white") +
  labs(title = "CONFERENCE COMPARISON", subtitle = "Conferences with the most titles since 1985") +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) 

#Plot Conferance Comparison ---END---

#Plot Seed Difference Information Gain Check ---BEGIN---

seeds_performance <- MNCAATourneyCompactResults %>%
  filter(Season >= 1985) %>%
  left_join(MNCAATourneySeeds, by = c("Season", "WTeamID" = "TeamID")) %>%
  left_join(MNCAATourneySeeds, by = c("Season", "LTeamID" = "TeamID")) %>%
  rename(WinnerSeed = Seed.x, LoserSeed = Seed.y) %>%
  mutate(winner_higher_seed = ifelse(WinnerSeed < LoserSeed, "Higher Seed Wins", ifelse(LoserSeed < WinnerSeed, "Lower Seed Wins", "Same Seed"))) %>%
  mutate(winner_higher_seed = factor(winner_higher_seed, levels = c("Lower Seed Wins", "Same Seed", "Higher Seed Wins")))

seeds_performance %>%
  ggplot(aes(x=TourneyRound, fill = winner_higher_seed)) +
  geom_bar(stat = "count", position = "fill", color = "grey") +
  scale_fill_manual(values = c("#FF6666", "blue", "#00FFFF")) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Proportion of games won by Higher Seed since 1985", subtitle = "Final Four and title game have same ranked teams") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = .8, size = 10))

#Plot Seed Difference Information Gain Check ---END---

#Plot ELO Ranking By Year Comparison ---BEGIN---

massey_pom_end <- MMasseyOrdinals %>%
  filter(SystemName == "POM") %>% # Select Pomeroy rankings
  group_by(Season, TeamID) %>%
  filter(RankingDayNum == max(RankingDayNum)) %>% ungroup() %>% # select the latest date of the season
  select(Season, TeamID, OrdinalRank)


rankings_tourney_analysis_pom <- MNCAATourneyCompactResults %>%
  filter(Season >= 2003) %>%
  left_join(massey_pom_end, by = c("Season", "WTeamID" = "TeamID")) %>%
  rename(WinnerRank = OrdinalRank) %>%
  left_join(massey_pom_end, by = c("Season", "LTeamID" = "TeamID")) %>%
  rename(LoserRank = OrdinalRank) %>%
  filter(DayNum >= 136) %>%
  # create a feature that classifies whether the winner was higher or lower ranked
  mutate(WinnerHigher_yes_no = ifelse(WinnerRank < LoserRank, "Winner Ranked Better", "Loser Ranked Better")) 


rankings_tourney_summary_pom <- rankings_tourney_analysis_pom %>%
  group_by(Season, WinnerHigher_yes_no) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), 4))

massey_sagarin_end <- MMasseyOrdinals %>%
  filter(SystemName == "SAG") %>% # Select Sagarin rankings
  group_by(Season, TeamID) %>%
  filter(RankingDayNum == max(RankingDayNum)) %>% ungroup() %>% # select the latest date of the season
  select(Season, TeamID, OrdinalRank)



rankings_tourney_analysis_sag <- MNCAATourneyCompactResults %>%
  filter(Season >= 2003) %>%
  left_join(massey_sagarin_end, by = c("Season", "WTeamID" = "TeamID")) %>%
  rename(WinnerRank = OrdinalRank) %>%
  left_join(massey_sagarin_end, by = c("Season", "LTeamID" = "TeamID")) %>%
  rename(LoserRank = OrdinalRank) %>%
  filter(DayNum >= 136) %>%
  # create a feature that classifies whether the winner was higher or lower ranked
  mutate(WinnerHigher_yes_no = ifelse(WinnerRank < LoserRank, "Winner Ranked Better", "Loser Ranked Better")) 


rankings_tourney_summary_sag <- rankings_tourney_analysis_sag %>%
  group_by(Season, WinnerHigher_yes_no) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), 4))


pom <- rankings_tourney_summary_pom %>%
  ggplot(aes(x= factor(Season), y= prop, fill = WinnerHigher_yes_no)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  geom_text(data = subset(rankings_tourney_summary_pom, WinnerHigher_yes_no == "Winner Ranked Better"), aes(label = percent(prop)), hjust = 1, colour = "white") +
  scale_fill_manual(values = c("lightgrey", "#FF6666")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Season") +
  ggtitle("ELO Ranking by Year System Comparison", subtitle = "Pomeroy Ranking Performance") +
  coord_flip() +
  theme(axis.title.x = element_blank())


sag <- rankings_tourney_summary_sag %>%
  ggplot(aes(x= factor(Season), y= prop, fill = WinnerHigher_yes_no)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  geom_text(data = subset(rankings_tourney_summary_sag, WinnerHigher_yes_no == "Winner Ranked Better"), aes(label = percent(prop)), hjust = 1, colour = "white") +
  scale_fill_manual(values = c("lightgrey", "#00FFFF")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "") +
  ggtitle("", subtitle = "Sagarin Ranking Performance") +
  coord_flip() +
  theme(axis.title.x = element_blank())


grid.arrange(pom, sag, ncol = 2)

gc()

#Plot ELO Ranking By Year Comparison ---END---

#Plot ELO Ranking System by ROUND Comparison ---BEGIN---

pom <- rankings_tourney_analysis_pom %>%
  ggplot(aes(x= TourneyRound, fill = WinnerHigher_yes_no)) +
  geom_bar(stat = "count", position = "fill", color = "grey") +
  scale_fill_manual(values = c("lightgrey", "#FF6666")) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Elo Ranking by Round System Comparison", subtitle = "Pomeroy Ranking Performance") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = .8, size = 10))


sag <- rankings_tourney_analysis_sag %>%
  ggplot(aes(x= TourneyRound, fill = WinnerHigher_yes_no)) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  scale_fill_manual(values = c("lightgrey", "#00FFFF")) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("", subtitle = "Sagarin Ranking Performance") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = .8, size = 10))


grid.arrange(pom, sag, ncol = 2)

#Plot ELO Ranking System by ROUND Comparison ---END---

#Plot Seed Most Title Winners ---BEGIN---

MNCAATourneySeeds$Seed <- as.integer(str_extract_all(MNCAATourneySeeds$Seed, "[0-9]+"))

ncaa_champs %>%
  select(Season, TeamID = WTeamID) %>%
  left_join(MNCAATourneySeeds, by = c("Season", "TeamID")) %>%
  count(Seed, sort = T) %>%
  mutate(Seed = as.character(Seed)) %>%
  ggplot(aes(x= reorder(Seed, n), y= n)) +
  geom_segment(aes(x= Seed, xend = Seed, y= 0, yend = n), color = "#FF6666", size = 1) +
  geom_point(size = 4, color = "#00FFFF") +
  scale_y_continuous(labels = c("0", "5", "10", "15", "20 Titles", "")) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Seeds that have won the most titles")


#Plot Seed Most Title Winners ---END---