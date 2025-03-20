# Thatch Phypers euclidian openness
import pandas as pd
import numpy as np
from scipy.spatial import distance_matrix
from thefuzz import process, fuzz

def filter_dataframe_after_snap(df):
  if 'frameType' not in df.columns:
    raise ValueError("DataFrame does not contain a 'frameType' column.")

  filtered_df = df[df['frameType'] == 'AFTER_SNAP']
  return filtered_df


# Function to compute nearest opponent distance
def compute_nearest_opponent_distance(frame):
    opponents = frame.groupby("club")["nflId"].unique()
    
    distances = []
    for _, player in frame.iterrows():
        # Extract only opponent players
        opponent_team = opponents.loc[opponents.index != player["club"]]
        opponent_ids = np.concatenate(opponent_team.values)

        # Filter opponent positions
        opponents_df = frame[frame["nflId"].isin(opponent_ids)][["x", "y", "nflId"]]
        
        if not opponents_df.empty:
            # Compute distances
            dists = np.sqrt((opponents_df["x"] - player["x"])**2 + (opponents_df["y"] - player["y"])**2)
            nearest_dist = dists.min()
        else:
            nearest_dist = np.nan  # No opponents found
        
        distances.append(nearest_dist)
    
    frame["nearest_opponent_dist"] = distances
    return frame


df = pd.read_csv("tracking_combined.csv")

# Filter the DataFrame
df = filter_dataframe_after_snap(df)

# Apply function per game, play, and frame, and RESET THE INDEX
df = df.groupby(["gameId", "playId", "frameId"]).apply(compute_nearest_opponent_distance).reset_index(drop=True)

# Now computing maximum distance from defender per play per game per player before pass occurs

# First, identify when passes occur in each play
pass_events = df[df['event'] == 'pass_forward'].copy()

# Get the timestamp of the pass for each gameId-playId combination
pass_times = pass_events.groupby(['gameId', 'playId'])['time'].min().reset_index()
pass_times.rename(columns={'time': 'pass_time'}, inplace=True)

# Merge this back to the main dataframe
df_with_pass_time = pd.merge(df, pass_times, on=['gameId', 'playId'], how='left')

# Filter to only include frames before the pass occurs
before_pass_df = df_with_pass_time[df_with_pass_time['time'] <= df_with_pass_time['pass_time']]

# Group by gameId, playId, nflId to find maximum nearest opponent distance before pass
max_dist_df = before_pass_df.groupby(['gameId', 'playId', 'nflId']).agg({
    'nearest_opponent_dist': 'max',
    'displayName': 'first', 
    'time': 'last',  # Last time before the pass
    'jerseyNumber': 'first',
    'club': 'first',
    'playDirection': 'first'
}).reset_index()

# Rename the column to match the requested output
max_dist_df.rename(columns={'nearest_opponent_dist': 'max_dist_before_pass'}, inplace=True)

# Select only the requested columns
result_df = max_dist_df[['gameId', 'playId', 'nflId', 'displayName', 'time', 
                         'jerseyNumber', 'club', 'playDirection', 'max_dist_before_pass']]

# joining to combine
df = result_df
combine = pd.read_csv("filtered_combine.csv")  # Combine dataset 

# Function to perform fuzzy matching
def fuzzy_merge(df, combine, df_key, combine_key, threshold=80):
    matched_names = []
    
    for name in df[df_key]:
        if pd.isna(name):  # Skip if the name is NaN
            matched_names.append(None)
            continue
        
        match = process.extractOne(name, combine[combine_key].dropna().tolist(), scorer=fuzz.ratio)  # Convert to list
        
        if match:  # Ensure match is found
            best_match, score = match[:2]  # Extract only the first two values (ignore index)
            if score >= threshold:
                matched_names.append(best_match)
            else:
                matched_names.append(None)
        else:
            matched_names.append(None)

    df["matched_name"] = matched_names
    return df.merge(combine, left_on="matched_name", right_on=combine_key, how="inner")

# Perform fuzzy merge on "displayName" (df) and "Player" (combine) using INNER JOIN
df = fuzzy_merge(df, combine, "displayName", "Player")

# Select final columns
final_df = df[[
    "gameId", "playId", "nflId", "displayName", "jerseyNumber", "club",
    "playDirection", "max_dist_before_pass", "Pos", "School", "Ht", "Wt",
    "X40yd", "X3Cone", "Shuttle", "YearDrafted"
]]

# Save the final dataframe
final_df.to_csv("euclidianOpenness.csv", index=False)

print("Fuzzy INNER join completed successfully and saved as euclidianOpenness.csv")