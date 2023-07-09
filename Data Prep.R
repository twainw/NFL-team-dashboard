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

season_yr <- 2022
pbp <- load_pbp(seasons = season_yr)
schedule <- load_schedules(seasons = season_yr)

schedule |> 
  clean_homeaway() |> 
  View()

#--------------------------------------------------------
          # Data for team preview page visuals #
#--------------------------------------------------------

#--------------------------
# Advanced Stats and Rank Summary
#--------------------------

calculate_adv_stats <- function(team_var, type_var, pts_var){
  
  # Step: Build and return advanced stats df
  stats_df <- pbp |> 
    filter(!is.na(epa), !is.na({{team_var}}), pass == 1 | rush == 1) |> 
    group_by(season, week, season_type, team = {{team_var}}, type = type_var) |>
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
    left_join(
      schedule |> 
        clean_homeaway() |> 
        select(season, team, week, game_id, opponent, team_score, opponent_score), 
              by = c("season", "week", "team")
              )
  
  # Step: return stats_df
  return(stats_df)
}

off_df <- calculate_adv_stats(posteam, "offense", team_score)
def_df <- calculate_adv_stats(defteam, "defense", opponent_score)

#--------------------------
# Export
#--------------------------

bind_rows(off_df, def_df) |> 
  mutate(join_key = paste(season, week, team, type, sep = '-')) -> off_def_stats

dim_team <- off_def_stats |> select(season:type, join_key) |> 
  inner_join(teams_colors_logos, by = c("team" = "team_abbr"))

off_def_stats |> write.csv("Data/off_def_adv_stats.csv")
dim_team |> write.csv("Data/dim_team.csv")

rm(off_df, def_df)

