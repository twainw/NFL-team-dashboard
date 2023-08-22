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

# Step: Load Teams

teams <- load_teams() |> filter(!team_abbr %in% c("LAR", "OAK", "STL", "SD"))

# Step: Load play-by-play

pbp <- load_participation(seasons = season_yr, include_pbp = T)

# Step: Load Schedules

schedule <- load_schedules(seasons = season_yr) |> 
  clean_homeaway()

# Step: Load Player Stats

player_stats <- load_player_stats(seasons = season_yr)

# Step: Brad's Alt Logos CSV

teams_alt_logos <- readr::read_csv("https://raw.githubusercontent.com/bcongelio/nfl_alt_logos/main/team_alt_logos.csv")

# Step: Season Standings

season_standing <- standings |> 
  filter(season == season_yr)

#--------------------------------------------------------
          # Data Prep #
#--------------------------------------------------------

#--------------------------
# By Week Summary
#--------------------------

calculate_adv_stats <- function(team_var, opp_var, type_var){
  
  # Step: Build and return advanced stats df
  stats_df <- pbp |> 
    filter(!is.na(epa), !is.na({{team_var}}), pass == 1 | rush == 1) |> 
    mutate(explosive_pass = ifelse(pass == 1 & yards_gained >= 20, 1, 0),
           explosive_run = ifelse(rush == 1 & yards_gained >= 15, 1, 0)) |> 
    group_by(season, week, season_type, team = {{team_var}}, 
             opponent = {{opp_var}}, type = type_var) |> 
    summarize(epa_per_play = sum(epa) / n()
              , passes = sum(pass)
              , interceptions = sum(interception, na.rm = T)
              , passing_yds = sum(passing_yards, na.rm = T)
              , dropback_epa = sum(epa*pass) / sum(pass)
              , dropback_sr = sum(success*pass) / sum(pass)
              , dropback_explosive = sum(explosive_pass, na.rm = T) / sum(pass)
              , rushes = sum(rush)
              , lost_fumbles = sum(fumble_lost, na.rm = T)
              , rushing_yds = sum(rushing_yards, na.rm = T)
              , rush_epa = sum(epa*rush) / sum(rush)
              , rush_sr = sum(success*rush) / sum(rush)
              , rush_explosive = sum(explosive_run, na.rm = T) / sum(rush)
              , sr = sum(success) / n()
              , tot_plays = sum(pass) + sum(rush)
              , tot_yds = sum(passing_yards, na.rm = T) + sum(rushing_yards, na.rm = T)
              , tot_tos = sum(interception, na.rm = T) + sum(fumble_lost, na.rm = T))|> 
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

off_stats_df <- calculate_adv_stats(posteam, defteam, "offense") |> 
  group_by(season, week) |> 
  mutate(across(c(epa_per_play, sr, dropback_epa, dropback_sr, 
                  dropback_explosive, rush_epa, rush_sr, rush_explosive)
                , ~ rank(-.x, ties.method = 'min')
                , .names = "{col}_rank")) |> 
  ungroup()

def_stats_df <- calculate_adv_stats(defteam, posteam, "defense") |> 
  group_by(season, week) |> 
  mutate(across(c(epa_per_play, sr, dropback_epa, dropback_sr, 
                  dropback_explosive, rush_epa, rush_sr, rush_explosive)
                , ~ rank(.x, ties.method = 'min')
                , .names = "{col}_rank")) |> 
  ungroup()

#--------------------------
# Effectiveness Summary
#--------------------------

calculate_eff_summary <- function(team_var, type_var, pts_var){
  
  # Step: Build and return advanced stats df
  stats_df <- pbp |> 
    filter(!is.na(epa), !is.na({{team_var}}), pass == 1 | rush == 1) |> 
    group_by(season, week, season_type, team = {{team_var}}, type = type_var) |> 
    summarize(total_epa = sum(epa),
              total_plays = n(), 
              total_success = sum(success), 
              total_pass_epa = sum(epa*pass),
              total_passes = sum(pass),
              total_pass_success = sum(success*pass),
              total_rush_epa = sum(epa*rush),
              total_rushes = sum(rush),
              total_rush_success = sum(success*rush)) |> 
    ungroup() |> 
    left_join(schedule |> 
                group_by(season, team, week) |> 
                summarize(pts = sum({{pts_var}})), 
              by = c("season", "team", "week")) |> 
    left_join(nflfastR::teams_colors_logos |> 
                transmute(team_abbr, team_logo = team_logo_espn), 
              by = c("team" = "team_abbr"))
  
  # Step: return stats_df
  return(stats_df)
}

off_eff_df <- calculate_eff_summary(posteam, "offense", team_score)
def_eff_df <- calculate_eff_summary(defteam, "defense", opponent_score)




