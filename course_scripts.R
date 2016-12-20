library(dplyr)
library(Lahman)
#Batting
head(Batting)

bdat <- Batting %>% 
  filter(AB > 500 & (HR >= 50 | SO < 25)) %>%
  select(playerID, HR, yearID) %>%
  arrange(desc(HR))
bdat

bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(career_HR=sum(HR, na.rm=TRUE)) %>%
  arrange(desc(career_HR))
bdat

bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(avg_season_H=round(mean(H, na.rm=TRUE), 2)) %>%
  arrange(desc(avg_season_H))
bdat

bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(max_career_HR=max(HR), min_career_SO=min(SO)) %>%
  arrange(desc(max_career_HR))
bdat

bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(number_of_records=n()) %>%
  arrange(desc(number_of_records))
bdat
