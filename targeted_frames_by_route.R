# Initialize an empty list to store data 
all_weeks_data <- list()

# Loop through weeks 1-9 to process route running frames data
for (week in 1:9) {
  # Read tracking data for the current week
  temp <- read.csv(paste0("data/route_frames_week_", week, ".csv"))
  all_weeks_data[[week]] <- temp
}

# route_running_frames holds data from all nine weeks of route_running_frames, created from data_merging code
# route_running_frames includes routeRan variable (only difference from data_merging)
route_running_frames <- do.call(rbind, all_weeks_data)

# Filtering route_running_frames to focus on targeted receivers
library(dplyr)
targeted_route_runners <-
  route_running_frames |> 
  filter(wasTargettedReceiver == 1)

# Table of routes ran
table(targeted_route_runners$routeRan)

# route_types holds each unique route type
route_types <- unique(targeted_route_runners$routeRan)

# route_data_frames list to hold data for each route type
route_data_frames <- list()

# Going through route types in tagetted_route_runners and adding each route to route_data_frames
for (route in route_types) {
  route_data_frames[[route]] <- targeted_route_runners[targeted_route_runners$routeRan == route, ]
}

# Count frames until pass towards targeted receiver for each play in each route type
count_frames_until_pass <- function(df) {
  df |> 
    group_by(playId) |> 
    mutate(first_pass_frame = ifelse(any(event == "pass_forward" | event == "pass_shovel"), min(frameId[event == "pass_forward" | event == "pass_shovel"], na.rm = TRUE), NA),
           frames_until_pass = ifelse(!is.na(first_pass_frame), first_pass_frame - min(frameId, na.rm = TRUE), NA)) |> 
    ungroup() |> 
    select(-first_pass_frame)
}

route_data_frames <- 
  lapply(route_data_frames,
         count_frames_until_pass)

# Retrieving each route type variable
for (route in names(route_data_frames)) {
  assign(paste0(tolower(route), "_df"),
         route_data_frames[[route]])
}

# Route summary shows the min + max number of frames until pass towards targeted receiver for each route type
# Also shows average number of frames until pass for each route, and number of plays for each route where a player was targeted 
route_summary <-
  route_data_frames |> 
  bind_rows(.id = "route_type") |> 
  group_by(route_type,
           playId) |> 
  slice(1) |> 
  ungroup() |> 
  group_by(route_type) |> 
  summarise(min_frames = min(frames_until_pass, na.rm = TRUE),
            max_frames = max(frames_until_pass, na.rm = TRUE),
            avg_frames = mean(frames_until_pass, na.rm = TRUE),
            plays = n())

# Display route_summary
# TODO: Check to make sure route_summary results are accurate
print(route_summary)

# Create targeted_route_running_frames
# Removes frames from original route_running_frames data not within targeting range for each route type 
# Join route_running_frames with route_summary to get min/max frames per route type
# Filter route_running_frames based on min/max frameId for each route type
targeted_route_running_frames <- 
  route_running_frames |> 
  inner_join(route_summary, 
             by = c("routeRan" = "route_type")) |> 
  group_by(routeRan, 
           playId) |> 
  mutate(start_frame = min(frameId, na.rm = TRUE),  # Get first frameId of the play
         end_frame = max(frameId, na.rm = TRUE),    # Get last frameId of the play
         min_valid_frame = start_frame + (min_frames),  
         max_valid_frame = pmin(start_frame + (max_frames), end_frame)) |>
  filter(frameId >= min_valid_frame & 
         frameId <= max_valid_frame) |> 
  select(-start_frame,
         -end_frame,
         -min_valid_frame,
         -max_valid_frame) |>
  ungroup()
## Removed 727,995 frames
## I filtered out frames for each route type where players were never targeted
## Could potentially do a more aggressive frame removal, find other ways to differentiate routes

write.csv(targeted_route_running_frames, 'targeted_route_frames.csv', row.names = F)
