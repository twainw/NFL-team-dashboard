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

# Step: Load Teams

teams <- load_teams() |> filter(!team_abbr %in% c("LA", "OAK", "STL", "SD"))

# Step: Load play-by-play

pbp <- load_pbp(seasons = season_yr)

# Step: Load Schedules

schedule <- load_schedules(seasons = season_yr) |> 
  clean_homeaway()

# Step: Load Player Stats

player_stats <- load_player_stats(seasons = season_yr)

# Step: Brad's Alt Logos CSV

teams_alt_logos <- readr::read_csv("https://raw.githubusercontent.com/bcongelio/nfl_alt_logos/main/team_alt_logos.csv")

#--------------------------------------------------------
          # Data Prep #
#--------------------------------------------------------

#--------------------------
# By Week Summary
#--------------------------

calculate_adv_stats <- function(team_var, opp_var, type_var, pts_var){
  
  # Step: Build and return advanced stats df
  stats_df <- pbp |> 
    filter(!is.na(epa), !is.na({{team_var}}), pass == 1 | rush == 1) |> 
    group_by(season, week, season_type, team = {{team_var}}, 
             opponent = {{opp_var}}, type = type_var) |> 
    summarize(epa_per_play = sum(epa) / n()
              , dropback_epa = sum(epa*pass) / sum(pass)
              , dropback_sr = sum(success*pass) / sum(pass)
              , rush_epa = sum(epa*rush) / sum(rush)
              , rush_sr = sum(success*rush) / sum(rush)
              , sr = sum(success) / n()) |> 
    ungroup() |> 
    left_join(schedule |> 
                select(season, team, week, team_score, opponent_score, team_rest, 
                       opponent_rest, spread_line) |> 
                mutate(result = case_when(
                  team_score < opponent_score ~ "L", 
                  team_score > opponent_score ~ "W", 
                  team_score == opponent_score ~ "T"
                )),
              by = c("season", "team", "week")) |> 
    left_join(nflfastR::teams_colors_logos |>
                transmute(team_abbr, opp_logo = team_logo_espn), 
              by = c("opponent" = "team_abbr")) |> 
    left_join(nflfastR::teams_colors_logos |> 
                transmute(team_abbr, team_logo = team_wordmark), 
              by = c("team" = "team_abbr")) |> 
    mutate(final_score = paste0(team_score, "-", opponent_score))
  
  # Step: return stats_df
  return(stats_df)
}

off_stats_df <- calculate_adv_stats(posteam, defteam, "offense", team_score)
def_stats_df <- calculate_adv_stats(defteam, posteam, "defense", opponent_score)







