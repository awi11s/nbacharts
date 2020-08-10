library(nbastatR)
library(dplyr)
library(ggplot2)
library(magick)
library(ggimage)
library(scales)

stats <- 
  game_logs(seasons = 2020, result_types = "player") %>% 
  select(idGame, namePlayer, pts, urlPlayerPhoto) %>% 
  filter(idGame > 0021901231)

stats <- stats %>% 
  mutate(stats, games = 1)

stats <- stats %>% 
  group_by(namePlayer, urlPlayerPhoto) %>% 
  summarise_at(vars(starts_with('pts'), starts_with('games')), sum)


stats <- stats %>% 
  arrange(desc(pts)) %>% 
  filter(pts > 127)


chart <-
  ggplot(stats, aes(x = pts, y = reorder(namePlayer, pts))) +
  geom_bar(stat = "identity", fill = "#FEAE46", width = 0.5) +
  geom_image(aes(image = urlPlayerPhoto), size = 0.111) +
  geom_text(aes(label=pts, family = "Palatino"), hjust=2.2, vjust=0.5, size=4) +
  ggtitle("Top Scorers from the Bubble") +
  xlab("Total Points") +
  ylab("Top 10 Performers") +
  theme_solarized() +
  theme(text = element_text(family = "Palatino"))

chart
