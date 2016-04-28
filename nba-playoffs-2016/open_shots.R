# Do teams who get open shots in the regular season do worse in the playoffs?

setwd("~/sports/nba-playoffs-2016/")

require(readr)
require(dplyr)
require(ggplot2)

# Read in data
reg_season_16 <- read_tsv("reg_season_2016.tsv")
reg_season_16$year <- 2016
reg_season_15 <- read_tsv("reg_season_2015.tsv")
reg_season_15$year <- 2015
reg_season_14 <- read_tsv("reg_season_2014.tsv")
reg_season_14$year <- 2014

reg_season <- rbind(reg_season_14, reg_season_15, reg_season_16)
rm(reg_season_14, reg_season_15, reg_season_16)

opp_reg_season_16 <- read_tsv("opp_reg_season_2016.tsv")
opp_reg_season_16$year <- 2016
opp_reg_season_15 <- read_tsv("opp_reg_season_2015.tsv")
opp_reg_season_15$year <- 2015
opp_reg_season_14 <- read_tsv("opp_reg_season_2014.tsv")
opp_reg_season_14$year <- 2014

opp_reg_season <- rbind(opp_reg_season_14, opp_reg_season_15, opp_reg_season_16)
rm(opp_reg_season_14, opp_reg_season_15, opp_reg_season_16)

playoffs_16 <- read_tsv("playoffs_2016.tsv")
playoffs_16$year <- 2016
playoffs_15 <- read_tsv("playoffs_2015.tsv")
playoffs_15$year <- 2015
playoffs_14 <- read_tsv("playoffs_2014.tsv")
playoffs_14$year <- 2014

playoffs <- rbind(playoffs_14, playoffs_15, playoffs_16)
rm(playoffs_14, playoffs_15, playoffs_16)

## Merge data and drop unnecessary variables
all_data <- playoffs %>% 
  left_join(reg_season, by=c('Team', 'year')) %>% 
  left_join(opp_reg_season, by=c('Team', 'year')) %>% 
  select(1, 8, 3:5, 10:12, 16:18) %>% 
  rename(team = Team, EFG_playoffs=EFG.x, qSQ_playoffs=`Situational EFG (qSQ).x`, qSI_playoffs=`EFG+ (qSI).x`,
         EFG_reg=EFG.y, qSQ_reg=`Situational EFG (qSQ).y`, qSI_reg=`EFG+ (qSI).y`,
         EFG_def_reg=EFG, qSQ_def_reg=`Situational EFG (qSQ)`, qSI_def_reg=`EFG+ (qSI)`
         )

# Calculate playoff opponent's qSE_def_reg
playoff_series <- read_csv("playoff_series_data.csv")

playoff_series$team[which(playoff_series$team=="BRK")] <- 'BKN'
playoff_series$team[which(playoff_series$team=="CHO")] <- 'CHA'
playoff_series$opp[which(playoff_series$opp=="BRK")] <- 'BKN'
playoff_series$opp[which(playoff_series$opp=="CHO")] <- 'CHA'

playoff_opp_def <- playoff_series %>% 
  left_join(all_data, by=c('opp' = 'team', 'year')) %>% 
  select(1:4, qSQ_def_reg) %>% 
  group_by(team, year) %>% 
  summarize(opp_qSQ_def=mean(qSQ_def_reg, w=games, na.rm=T))

all_data <- all_data %>% left_join(playoff_opp_def, by=c('team', 'year'))

summary(lm(data=all_data, formula=qSQ_playoffs~qSQ_reg+opp_qSQ_def))

all_data %>% 
  ggplot(aes(opp_qSQ_def, qSQ_playoffs)) +
  geom_point() + geom_smooth(method="lm") +
  xlab("Opponent's Allowed Shot Quality On Defense (Regular Season)") +
  ylab("Shot Quality (Playoffs)") +
  ggtitle("Playoff Shot Quality v. Opponents Allowed Shot Quality\n 2014-2016 seasons")

