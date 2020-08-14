library(nbastatR)
library(dplyr)
library(ggplot2)
library(magick)
library(ggimage)
library(ggthemes)
library(scales)

#gathering stats for players and filtering games only from Orlando Bubble.

stats <- 
  game_logs(seasons = 2020, result_types = "player") %>% 
  select(idGame, namePlayer, pts, urlPlayerPhoto) %>% 
  filter(idGame > 0021901231)

#Creating a new column for games that players have played, 
#grouping every instance a player is listed multiple times into a single row while summing their points,
#and only selecting the top 10 players

final_table <- stats %>% 
  mutate(stats, games = 1) %>% 
  group_by(namePlayer, urlPlayerPhoto) %>% 
  summarise_at(vars(starts_with('pts'), starts_with('games')), sum) %>% 
  arrange(desc(pts)) %>% 
  filter(pts > 160)

  
#plotting the bar chart using ggplot2 and ggthemes

chart <-
  ggplot(final_table, aes(x = pts, y = reorder(namePlayer, pts))) +
  geom_bar(stat = "identity", fill = "#FEAE46", width = 0.5) +
  geom_image(aes(image = urlPlayerPhoto), size = 0.111) +
  geom_text(aes(label=(round(pts/games)), family = "Palatino"), hjust=2.2, vjust=0.5, size=4) +
  ggtitle("Top Scorers from the Bubble") +
  xlab("Total Points") +
  ylab("Top 10 Performers") +
  theme_solarized() +
  theme(text = element_text(family = "Palatino"))

chart