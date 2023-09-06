#--------------------------------------------------------
# Preliminaries  #
#--------------------------------------------------------

#--------------------------
# Packages
#--------------------------

library(tidyverse)
library(nflreadr)
library(rlang)
library(nflfastR)

#--------------------------
# Data
#--------------------------

# Step: Load Snap Counts

snaps <- load_snap_counts(seasons = season_yr)

# Step: Load Player Stats

stats <- load_player_stats(season_yr)

# Step: Load Rosters

rosters <- load_rosters(season_yr)

#--------------------------------------------------------
# Data Prep #
#--------------------------------------------------------

#--------------------------
# By Week Summary
#--------------------------

pbp |>
  filter(!is.na(epa), !is.na(posteam), rush == 1) |> 
  group_by(season, week, season_type, posteam, rusher_player_name, rusher, rusher_player_id) |> 
  summarize(rushes = sum(rush)
            , tackled_loss = sum(tackled_for_loss, na.rm = T)
            , fumbles = sum(fumble, na.rm = T)
            , lost_fumbles = sum(fumble_lost, na.rm = T)
            
            , rushing_yds = sum(rushing_yards, na.rm = T)
            , rush_tds = sum(rush_touchdown, na.rm = T)
            , rush_sr = sum(success*rush) / sum(rush)
            , rush_epa = sum(epa*rush) / sum(rush)
            
            , light_boxes = sum(defenders_in_box <= 6, na.rm = T)
            , heavy_boxes = sum(defenders_in_box >= 8, na.rm = T)
  ) |> 
  View()
  