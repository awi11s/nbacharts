library(nbastatR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(showtext)
library(magick)
library(ggthemr)

showtext_auto()

set.seed(123)


cp3pics <- 
  nba_players() %>% 
  filter(namePlayer == "Chris Paul")
  
newcp3 <-
  teams_shots(teams = "Oklahoma City Thunder", seasons = 2020) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

rookcp3 <- 
  teams_shots(team_ids = 1610612740, seasons = 2006) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

sophcp3 <-
  teams_shots(team_ids = 1610612740, seasons = 2007) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

sophcp3 <- 
  sophcp3 %>% 
  full_join(rookcp3)

juncp3 <-
  teams_shots(team_ids = 1610612740, seasons = 2008) %>% 
  select(yearSeason, slugSeason, namePlayer, zoneBasic, isShotMade) %>% 
  filter(namePlayer == "Chris Paul")

oldcp3 <-
  juncp3 %>% 
  full_join(sophcp3)


cp3full <- 
  newcp3 %>% 
  full_join(oldcp3) %>% 
  filter(zoneBasic == "Mid-Range") %>%  
  mutate(cp3full, shotmake = ifelse(isShotMade == TRUE, 1, 0)) %>% 
  mutate(cp3full, shottot = 1)
  
cp3full <- 
  cp3full %>%  
  group_by(slugSeason, namePlayer) %>% 
  summarise_at(vars(starts_with('shot')), sum) %>% 
  mutate()

ggthemr('dust')

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
  

