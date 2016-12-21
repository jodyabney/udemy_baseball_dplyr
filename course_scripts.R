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

# Section 4, Lecture 18 Inner Join
bdat <- inner_join(Batting, Master, by=c("playerID")) %>%
  filter(playerID=="ruthba01" | playerID=="aaronha01")
bdat

bdat <- Batting %>%
  filter(playerID=="ruthba01" | playerID=="aaronha01")
bdat <- inner_join(bdat, Master, by=c("playerID"))
bdat

# Section 4, Lecture 19 A query with inner join
bdat <- Batting %>%
  select(playerID, teamID, yearID, HR)
head(bdat)

bdat <- inner_join(bdat, Master, by=c("playerID")) %>%
  select(nameFirst, nameLast, teamID, yearID, HR)
head(bdat)

# Section 4, Lecture 21 inner join on multiple fields
bdat <- Batting %>%
  filter(playerID=="ruthba01") %>%
  select(playerID, teamID, yearID, HR)
bdat

bdat <- inner_join(bdat, Teams, by=c("teamID", "yearID")) %>%
  select(playerID, name, yearID, player_HR=HR.x)
bdat

# Sectoin 4, Lecture 23 inner join on three tables
bdat <- Batting %>%
  filter(playerID=="ruthba01") %>%
  select(playerID, teamID, yearID, HR)
bdat

bdat <- inner_join(bdat, Master, by=c("playerID")) %>%
  select(nameFirst, nameLast, teamID, yearID, HR)
bdat

bdat <- inner_join(bdat, Teams, by=c("teamID", "yearID")) %>%
  select(nameFirst, nameLast, name, yearID, player_HR=HR.x)
bdat

# Section 4, Lecture 25 
bdat <- Batting %>%
  group_by(playerID) %>%
  summarise(career_HR=sum(HR, na.rm=TRUE))
bdat

bdat <- inner_join(bdat, Master, by=c("playerID")) %>%
  select(nameFirst, nameLast, career_HR)
bdat

# with one query
bdat <- inner_join(Batting, Master, by=c("playerID")) %>%
  group_by(playerID) %>%
  summarise(first_name=nameFirst[1], last_name=nameLast[1], career_HR=sum(HR, na.rm=TRUE)) %>%
  select(first_name, last_name, career_HR)
bdat

#Section 5, Lecture 2
bdat <- Teams %>%
  #filter(park=="Petco Park") %>%
  select(teamID, yearID, park)
bdat

bdat <- inner_join(bdat, Batting, by=c("teamID", "yearID")) %>%
  select(playerID, teamID, yearID, park) %>%
  filter(park=="Petco Park") %>%
  group_by(playerID) %>%
  select(playerID, teamID, yearID, park)
bdat

bdat <- inner_join(bdat, Master, by=c("playerID")) %>%
  group_by(playerID) %>%
  summarise(first_name=nameFirst[1], last_name=nameLast[1]) %>%
  select(first_name, last_name)
bdat

# Section 5, Lecture 4
bdat <- Salaries %>%
  group_by(playerID) %>%
  summarise(career_avg_salary=mean(salary)) %>%
  filter(career_avg_salary > 1000000)
bdat

bdat <- inner_join(bdat, Master) %>%
  filter(nameFirst=="Bob") %>%
  select(nameFirst, nameLast)
bdat

