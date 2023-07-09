#--------------------------------------------------------
          # Preliminaries  #
#--------------------------------------------------------

#--------------------------
# Packages
#--------------------------

library(tidyverse)
library(nflreadr)
library(rlang)

#--------------------------
# Data
#--------------------------

season_yr <- 2022
pbp <- load_pbp(seasons = season_yr)
schedule <- load_schedules(seasons = season_yr)

#--------------------------------------------------------
          # Data for team preview page visuals #
#--------------------------------------------------------

#--------------------------
# Function to calculate
# offense and defense stats
#--------------------------

calculate_adv_stats <- function(team_var, type_var, pts_var){
  
  # Step: Build pts_df
  pts_df <- schedule |> 
    clean_homeaway() |>
    mutate(season_type = ifelse(game_type == "REG", "REG", "POST")) |> 
    group_by(season, season_type, team) |> 
    summarize(total_pts = sum({{pts_var}})) |> 
    ungroup()
  
  # Step: Build and return advanced stats df
  pbp |> 
    filter(!is.na(epa), !is.na({{team_var}}), pass == 1 | rush == 1) |> 
    group_by(season, season_type, team = {{team_var}}, type = type_var) |>
    summarize(total_plays = n()
              , total_epa = sum(epa)
              , total_success = sum(success)
              , total_dropbacks = sum(pass)
              , total_dropback_epa = sum(epa*pass)
              , total_dropback_success = sum(success * pass)
              , total_rushes = sum(rush)
              , total_rush_epa = sum(epa*rush)
              , total_rush_success = sum(success*rush)) |> 
    ungroup() |> 
    left_join(pts_df
              , by = c("season", "season_type", "team")) %>%
    return()
}

off_df <- calculate_adv_stats(posteam, "offense", team_score)
def_df <- calculate_adv_stats(defteam, "defense", opponent_score)

#--------------------------
# Bind offense and defense
#--------------------------

bind_rows(off_df, def_df) |> write.csv("Data/off_def_adv_stats.csv")
rm(off_df, def_df)



