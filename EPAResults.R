#epa results

library(dplyr)
library(purrr)
library(broom)

#loading datasets
route_runners_combine <- read.csv("route_runners_combine.csv")
player_play <- read.csv('player_play.csv')
players <- read.csv('players.csv')
plays <- read.csv('plays.csv')

#collect metrics for players to tell who was targetted
targetted <- player_play |> select(nflId, playId, gameId, teamAbbr, wasTargettedReceiver, receivingYards, routeRan)

#combine with route_runners_combine, and create column of position for regression later
rr_combine <- targetted |>   right_join(
  y = route_runners_combine,
  by = "nflId",
  relationship = "many-to-many"
) |> mutate(position = case_when(
  position_WR == 1 ~ "WR",
  position_RB == 1 ~ "RB",
  position_TE == 1 ~ "TE",
  TRUE ~ NA_character_ 
))

#collect epa per play
epa <- plays |> select(playId, gameId, expectedPointsAdded)

#join with rr_combine and set epa to 0 for players on plays where they were not targetted
EPAdf <- epa |> right_join(
  y = rr_combine,
  by = c("gameId", "playId")
) |> mutate(expectedPointsAdded = ifelse(wasTargettedReceiver == 0, 0, expectedPointsAdded))

#calculate average EPA per player
averageEPA <- EPAdf |>
  group_by(nflId) |>
  mutate(average_expectedPointsAdded = mean(expectedPointsAdded, na.rm = TRUE)) |>
  ungroup() |>
  distinct(nflId, .keep_all = TRUE)

#calculate average EPA per player per route type
averageEPARoute <- EPAdf |>
  group_by(nflId, routeRan) |>
  mutate(average_expectedPointsAdded = mean(expectedPointsAdded, na.rm = TRUE)) |>
  ungroup() |>
  distinct(nflId, routeRan, .keep_all = TRUE) 

head(averageEPARoute)

# fitting and packaging results

unique_routes <- unique(averageEPARoute$routeRan) |> na.omit()
unique_positions <- unique(averageEPARoute$position)

# Create an empty list to store the subset dataframes
routeRegressionList <- list()

# Loop through each unique route and create a subset dataframe for each
for (route in unique_routes) {
  for (position in unique_positions){
    subset_df <- averageEPARoute[averageEPARoute$routeRan == route & averageEPARoute$position == position, ]
    subset_name <- paste0(route, "_", position) 
    routeRegressionList[[subset_name]] <- subset_df 
  }
}

#creating list to store results
resultsList <- list()

#iterating through all models
for (subset_name in names(routeRegressionList)) {
    route <- routeRegressionList[[subset_name]]
    EPArModel <- lm(route$average_expectedPointsAdded ~ route$X40yd + route$X3Cone + route$Shuttle + route$weight + route$total_inches)
    print(subset_name)
    print(summary(EPArModel))
}

# Create an empty list to store the subset dataframes
positionRegressionList <- list()

# Loop through each unique route and create a subset dataframe for each
for (position in unique_positions){
  subset_df <- averageEPARoute[averageEPARoute$position == position, ]
  subset_name <- paste0(position) 
  positionRegressionList[[subset_name]] <- subset_df 
}


#iterating through all models
for (subset_name in names(positionRegressionList)) {
  route <- positionRegressionList[[subset_name]]
  EPArModel <- lm(route$average_expectedPointsAdded ~ route$X40yd + route$X3Cone + route$Shuttle + route$weight + route$total_inches)
  print(subset_name)
  print(summary(EPArModel))
}



#finding average of coefficients
# Initialize an empty list to store coefficients by position
position_coefficients <- list()

# Iterate over each dataframe in routeRegressionList
for (route_name in names(routeRegressionList)) {
  route <- routeRegressionList[[route_name]]
  
  # Ensure 'route' is a valid dataframe
  if (!is.data.frame(route) || nrow(route) == 0) {
    warning(paste("Skipping", route_name, "because it is not a valid dataframe or is empty."))
    next
  }
  
  # Extract position from route_name (assumes format like "FLAT_WR", "WHEEL_TE", etc.)
  position <- sub(".*_(WR|TE|RB)$", "\\1", route_name)
  
  # Check if required columns exist
  required_columns <- c("average_expectedPointsAdded", "X40yd", "X3Cone", "Shuttle", "weight", "total_inches")
  if (!all(required_columns %in% colnames(route))) {
    warning(paste("Skipping", route_name, "due to missing required columns."))
    next
  }
  
  # Run linear regression
  model <- lm(average_expectedPointsAdded ~ X40yd + X3Cone + Shuttle + weight + total_inches, data = route)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Store coefficients under the correct position
  if (!position %in% names(position_coefficients)) {
    position_coefficients[[position]] <- list()
  }
  
  position_coefficients[[position]][[route_name]] <- coefs
}

# Compute average coefficients for each position
average_position_coefficients <- list()

for (position in names(position_coefficients)) {
  # Convert list of coefficients to a dataframe
  coef_df <- do.call(rbind, position_coefficients[[position]])
  
  # Compute mean for each coefficient
  avg_coefs <- colMeans(coef_df, na.rm = TRUE)
  
  # Store result
  average_position_coefficients[[position]] <- avg_coefs
}

# Print the final averaged coefficients for WR, TE, RB
print(average_position_coefficients)



library(corrplot)

corrplot(cor(averageEPARoute))










# graphing
ggplot(averageEPA, aes(x = X3Cone, y = average_expectedPointsAdded, color = position)) +
  geom_point() +  # Add points for each data point
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Average EPA vs. X3Cone",
    x = "X3Cone Time",
    y = "Average EPA"
  ) +
  theme_minimal() # Optional: Use a minimal theme for cleaner look

ggplot(averageEPA, aes(x = X40yd, y = average_expectedPointsAdded, color = position)) +
  geom_point() +  # Add points for each data point
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Average EPA vs. 40yd Dash",
    x = "X3Cone Time",
    y = "Average EPA"
  ) +
  theme_minimal() # Optional: Use a minimal theme for cleaner look

ggplot(averageEPA, aes(x = Shuttle, y = average_expectedPointsAdded, color = position)) +
  geom_point() +  # Add points for each data point
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Average EPA vs. Shuttle Run",
    x = "X3Cone Time",
    y = "Average EPA"
  ) +
  theme_minimal() # Optional: Use a minimal theme for cleaner look

faceted_data <- averageEPA %>%
  select(average_expectedPointsAdded, X40yd, X3Cone, Shuttle, position) %>%
  pivot_longer(cols = c(X40yd, X3Cone, Shuttle), names_to = "combine_stat", values_to = "combine_value")

# 2. Create the faceted plot
ggplot(faceted_data, aes(x = combine_value, y = average_expectedPointsAdded, color = position)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ combine_stat, scales = "free_x") + # Facet by combine_stat
  labs(
    title = "Average EPA vs. Combine Drill",
    x = "Combine Drill Time",
    y = "Average EPA per play",
    color = "Position"
  ) +
  theme_minimal()



#split between positions for regression
#wrEPA <- averageEPA |> filter(position_WR == 1)
#rbEPA <- averageEPA |> filter(position_RB == 1)
#teEPA <- averageEPA |> filter(position_TE == 1)

#wrEPAModel <- lm(wrEPA$average_expectedPointsAdded ~ wrEPA$X40yd + wrEPA$X3Cone + wrEPA$Shuttle + wrEPA$weight + wrEPA$total_inches)
#teEPAModel <- lm(teEPA$average_expectedPointsAdded ~ teEPA$X40yd + teEPA$X3Cone + teEPA$Shuttle + teEPA$weight + teEPA$total_inches)
#rbEPAModel <- lm(rbEPA$average_expectedPointsAdded ~ rbEPA$X40yd + rbEPA$X3Cone + rbEPA$Shuttle + rbEPA$weight + rbEPA$total_inches)
#summary(wrEPAModel)
#summary(teEPAModel)
#summary(rbEPAModel)

#split between route ran and position
#wrEPAr <- averageEPARoute |> filter(position_WR == 1)
#rbEPAr <- averageEPARoute |> filter(position_RB == 1)
#teEPAr <- averageEPARoute |> filter(position_TE == 1)

#wrEPArModel <- lm(wrEPAr$average_expectedPointsAdded ~ wrEPAr$X40yd + wrEPAr$X3Cone + wrEPAr$Shuttle + wrEPAr$weight + wrEPAr$total_inches + wrEPAr$routeRan)
#teEPArModel <- lm(teEPAr$average_expectedPointsAdded ~ teEPAr$X40yd + teEPAr$X3Cone + teEPAr$Shuttle + teEPAr$weight + teEPAr$total_inches + teEPAr$routeRan)
#rbEPArModel <- lm(rbEPAr$average_expectedPointsAdded ~ rbEPAr$X40yd + rbEPAr$X3Cone + rbEPAr$Shuttle + rbEPAr$weight + rbEPAr$total_inches + rbEPAr$routeRan)
#summary(wrEPArModel)
#summary(teEPArModel)
#summary(rbEPArModel)


