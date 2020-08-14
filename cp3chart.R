library(nbastatR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(ggthemr)

#getting mid-range shooting statistics from Chris Paul's 2019-2020 season

newcp3 <-
  teams_shots(teams = "Oklahoma City Thunder", seasons = 2020) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

#getting mid-range shooting statistics from Chris Paul's 1st season

rookcp3 <- 
  teams_shots(team_ids = 1610612740, seasons = 2006) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

#getting mid-range shooting statistics from Chris Paul's second season

sophcp3 <-
  teams_shots(team_ids = 1610612740, seasons = 2007) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

#getting mid-range shooting statistics from Chris Paul's 3rd season

juncp3 <-
  teams_shots(team_ids = 1610612740, seasons = 2008) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

#joining Chris Paul's stats from first two season into one table

oldcp3 <- 
  sophcp3 %>% 
  full_join(rookcp3)

#joining Chris Paul's stats from third season and this past season

newercp3 <-
  juncp3 %>% 
  full_join(newcp3)

#taking the new and old tables and joining them together.
#also adding two new columns to track made shots and attempted shots

cp3full <- 
  newercp3 %>% 
  full_join(oldcp3) %>% 
  filter(zoneBasic == "Mid-Range") %>%  
  mutate(cp3full, shotmake = ifelse(isShotMade == TRUE, 1, 0)) %>% 
  mutate(cp3full, shottot = 1)
  
#finalizing the table by grouping together every row into one for each year
#summing together the values created in the last function to find total mid-range makes and misses
#creating a new row which is the average of mid-range shots in each year

cp3full <- 
  cp3full %>%  
  group_by(slugSeason, namePlayer) %>% 
  summarise_at(vars(starts_with('shot')), sum) %>% 
  mutate(cp3full, avg = paste(round((shotmake/shottot) * 100), '%'))

#adding a theme for the plot

ggthemr('dust')

#plotting data

chart <-
  ggplot(cp3full, aes(x = slugSeason, y = avg)) +
  geom_bar(aes(fill = slugSeason), stat = "identity", position = "dodge2") +
  scale_fill_manual(values = c("#01D1CA","#01D1CA","#01D1CA","#FE8839"), guide = FALSE) +
  ggtitle("Chris Paul Mid-Range") +
  xlab("Seasons") +
  ylab("Mid-Range FG%") +
  geom_text(aes(label = cp3full$avg, vjust = -0.7)) +
  theme(text=element_text(size=16,  family="Palatino")) 
 

chart
  

