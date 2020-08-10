library(nbastatR)
library(dplyr)
library(ballr)
library(ggpubr)
library(purrr)

duncandrib <- 
  teams_shots(teams = "Miami Heat", seasons = 2020) %>% 
  filter(namePlayer == "Duncan Robinson" & (typeAction == "Pullup Jump shot" | typeAction == "Step Back Jump shot"))

duncanspot <-
  teams_shots(teams = "Miami Heat", seasons = 2020) %>% 
  filter(namePlayer == "Duncan Robinson" & (typeAction == "Jump Shot"))

jjdrib <- 
  teams_shots(teams = "New Orleans Pelicans", seasons = 2020) %>% 
  filter(namePlayer == "JJ Redick" & (typeAction == "Pullup Jump shot" | typeAction == "Step Back Jump shot"))

jjspot <-
  teams_shots(teams = "New Orleans Pelicans", seasons = 2020) %>% 
  filter(namePlayer == "JJ Redick" & (typeAction == "Jump Shot"))

jjspot <-
  jjspot %>% 
  mutate(jjspot, shotmake = ifelse(isShotMade == TRUE, 1, 0)) %>% 
  mutate(jjspot, shottot = 1) 
  


source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")

plot_court()
court_points <-
  court_points %>% 
  mutate_if(is.numeric, ~.*10)

DRdribchart <- 
  ggplot(duncandrib, aes(x = locationX, y = locationY + 45, color = typeEvent, size = distanceShot)) +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1) +
  coord_equal() +
  scale_colour_manual(values = c("#12A0FE", "#FE4C4C")) +
  geom_point(alpha = 0.6) +
  xlim(-260, 260) +
  ggtitle("Duncan Robinson Off-the-Dribble in 2019-20") +
  theme(text = element_text(size = 11, family = "Palatino"),
        panel.grid = element_blank(),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        legend.position = "none") +
  annotate(geom = "text", x = -20, y = 400, label = "44.5%", hjust = "left", size = 11, family = "Palatino")

DRspotchart <-
  ggplot(duncanspot, aes(x = locationX, y = locationY + 45, color = typeEvent, size = distanceShot)) +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1) +
  coord_equal() +
  scale_colour_manual(values = c("#12A0FE", "#FE4C4C")) +
  geom_point(alpha = 0.6) +
  xlim(-260, 260) +
  ggtitle("Duncan Robinson Catch and Shoot in 2019-20") +
  theme(text = element_text(size = 11, family = "Palatino"),
        panel.grid = element_blank(),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        legend.position = "none") +
  annotate(geom = "text", x = -20, y = 400, label = "44.5%", hjust = "left", size = 11, family = "Palatino")

JJdribchart <-
  ggplot(jjdrib, aes(x = locationX, y = locationY + 45, color = typeEvent, size = distanceShot)) +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1) +
  coord_equal() +
  scale_colour_manual(values = c("#12A0FE", "#FE4C4C")) +
  geom_point(alpha = 0.6) +
  xlim(-260, 260) +
  ggtitle("JJ Redick Off-The-Dribble in 2019-20") +
  theme(text = element_text(size = 11, family = "Palatino"),
        panel.grid = element_blank(),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        legend.position = "none") +
  annotate(geom = "text", x = -20, y = 400, label = "48.1%", hjust = "left", size = 11, family = "Palatino")

JJspotchart <-
  ggplot(jjspot, aes(x = locationX, y = locationY + 45, color = typeEvent, size = distanceShot)) +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1) +
  coord_equal() +
  scale_colour_manual(values = c("#12A0FE", "#FE4C4C")) +
  geom_point(alpha = 0.6) +
  xlim(-260, 260) +
  ggtitle("JJ Redick Catch and Shoot in 2019-20") +
  theme(text = element_text(size = 11, family = "Palatino"),
        panel.grid = element_blank(),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        legend.position = "none") +
  annotate(geom = "text", x = -20, y = 400, label = "42.9%", hjust = "left", size = 11, family = "Palatino")
             


graph <-
  ggarrange(DRdribchart, 
            DRspotchart, 
            JJdribchart, JJspotchart, 
            ncol = 2, 
            nrow = 2) +
  theme_pubr(legend = "none", border = TRUE)
graph
  
