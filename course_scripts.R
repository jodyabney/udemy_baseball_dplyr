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

# Section 3, Lecture 14 WHERE vs HAVING
bdat <- Batting %>%
  filter(AB >= 400) %>%
  group_by(playerID) %>%
  summarise(min_career_SO=min(SO, na.rm=TRUE)) %>%
  filter(min_career_SO < 20) %>%
  arrange(min_career_SO)
bdat

# Section 3, Lecture 15 Batting Avg for Record
bdat <- Batting %>%
  filter(AB >= 400) %>%
  mutate(batting_avg=round(H/AB, 3)) %>%
  select(playerID, batting_avg, yearID) %>%
  arrange(desc(batting_avg))
head(bdat)

# Section 3, Lecture 16 Career Batting Avg
bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(career_AB=sum(AB, na.rm=TRUE), career_H=sum(H, na.rm=TRUE)) %>%
  filter(career_AB > 1000) %>%
  mutate(career_batting_avg=round(career_H/career_AB, 3)) %>%
  select(playerID, career_batting_avg) %>%
  arrange(desc(career_batting_avg))
head(bdat)


