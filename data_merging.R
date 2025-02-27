library(tidyverse)
library(plotly)
library(janitor)

player_play <- read.csv('data/player_play.csv')
players <- read.csv('data/players.csv')
plays <- read.csv('data/plays.csv')

combine <- read.csv('data/filtered_combine.csv')

# Loading all tracking data - don't run this
rbind(
  read.csv('data/tracking_week_1.csv'),
  read.csv('data/tracking_week_2.csv'),
  read.csv('data/tracking_week_3.csv'),
  read.csv('data/tracking_week_4.csv'),
  read.csv('data/tracking_week_5.csv'),
  read.csv('data/tracking_week_6.csv'),
  read.csv('data/tracking_week_7.csv'),
  read.csv('data/tracking_week_8.csv'),
  read.csv('data/tracking_week_9.csv'),
) -> tracking

# getting the different types of routes ran
table(player_play$routeRan)

# get information on every player that ran a route in weeks 1-9
player_play |> 
  filter(
    !is.na(wasRunningRoute),
    wasRunningRoute == 1
  ) |> 
  dplyr::select(nflId) |> 
  group_by(nflId) |> 
  summarize(n_routes = n()) |> 
  left_join(
    y = players,
    by = "nflId"
  ) -> route_runners # contains id, height, weight, birthday, college, position and name
# the display name column is First Last

nrow(route_runners) # 480 route runners

# positions of all players that ran a route
table(route_runners$position)

library(fuzzyjoin)

# 
stringdist_left_join(
  x = route_runners,
  y = combine,
  by = c('displayName' = 'Player'),
  distance_col = 'string_dist'
) |> 
  filter(is.na(X)) |> arrange(desc(n_routes)) |> View()




# Week 1 example
#############################################
week1 <- read.csv('data/tracking_week_1.csv')
# get gameIds from week 1
week1_gameIds <- week1 |> pull(gameId) |> unique()

# filter tracking data to frames by route runners
# after the snap
player_play |> 
  filter(
    wasRunningRoute == 1,
    gameId %in% week1_gameIds
  ) |> 
  dplyr::select(gameId, playId, nflId, wasRunningRoute) |> 
  left_join(
    week1,
    by = c("gameId", "playId", "nflId")
  ) |> 
  filter(frameType == 'AFTER_SNAP') -> route_running_frames



### Plotting Example
###############################################
library(plotly)
route_running_frames |> 
  filter(gameId == route_running_frames$gameId[1], 
         playId == route_running_frames$playId[1]) |>
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~frameId,
    mode = 'markers',
    text = ~jerseyNumber,
    hoverinfo = "text",
    marker = list(size = 10),
    type = 'scatter'
  ) |> 
  layout(
    title = "Test"
  ) 



