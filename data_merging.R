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
  dplyr::select(gameId, playId, nflId, wasRunningRoute, wasTargettedReceiver) |> 
  left_join(
    week1,
    by = c("gameId", "playId", "nflId")
  ) |> 
  filter(frameType == 'AFTER_SNAP') -> route_running_frames


# each play location of ball at snap
week1 |> 
  filter(event == 'ball_snap',
         club == 'football') |>
  select(gameId, playId, playDirection, x, y) -> week1_ball_loc

write.csv(week1_ball_loc, 'ball_locations.csv', row.names)


route_running_frames$event |> table()
route_ending_events <- c("pass_arrived", "pass_shovel", "qb_sack", "run", "qb_strip_sack", "pass_tipped", "pass_outcome_interception", "pass_outcome_caught", "pass_outcome_incomplete", "dropped_pass", "fumble", "handoff")
route_running_frames |> 
  mutate(route_end_event = ifelse((event == 'pass_forward' & wasTargettedReceiver == 0) | (event %in% route_ending_events), TRUE, NA)) |> 
  group_by(gameId, playId, nflId) |> 
  fill(route_end_event) |>
  filter(is.na(route_end_event)) |> # TODO: test to see if any plays are dropped
  select(-route_end_event) -> filtered_route_frames

write.csv(filtered_route_frames, 'route_frames.csv', row.names=F)




### Plotting Example
###############################################
library(plotly)
route_running_frames |> 
  filter(gameId == 2022090800, 
         playId == 56) |>
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~frameId,
    mode = 'markers',
    text = ~nflId,
    hoverinfo = "text",
    marker = list(size = 10),
    type = 'scatter'
  ) |> 
  layout(
    title = "Test"
  ) 


