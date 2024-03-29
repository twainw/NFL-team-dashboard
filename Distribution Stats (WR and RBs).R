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

# Step: load charting data

charting <- load_ftn_charting(season_yr)

# Step: load snap counts

snaps <- load_snap_counts(seasons = season_yr)

#--------------------------
# Data prep
#--------------------------

# Step: QC target summary, by receiver and game, built from pbp w player stats
# done, to compute accurate counts of charting stats

"

# Step: reconcile player stats and a summary table built from pbp_w_charting
# so that I can pull accurate charting numbers

### Code chunk to play-around (and find the correct set of) filters, and view
### a players pbp data in a given game

pbp_w_charting |> 
  filter(!is.na(epa), !is.na(posteam), pass == 1, two_point_attempt != 1, play_type != 'no_play') |>
  filter(posteam == 'KC', 
         week == 21,
         receiver_player_id == '00-0037197') |> 
  View()

### Code chunk to view differences in total targets and receptions between 
### player stats data and the summary built from pbp_w_charting

summary_from_pbp <- pbp_w_charting |> 
  filter(!is.na(epa), !is.na(posteam), pass == 1, two_point_attempt != 1, play_type != 'no_play') |>
  group_by(posteam, week, receiver_player_id, receiver_player_name) |> 
  summarise(targets_pbp = n(), 
            receptions_pbp = sum(complete_pass)) |> 
  ungroup() |> 
  filter(!is.na(receiver_player_id)) |> 
  left_join(player_stats |> select(player_id, week, targets, receptions), 
            by = c('receiver_player_id' = 'player_id', 'week')) |> 
  mutate(targets_diff = targets_pbp - targets, 
         receptions_diff = receptions_pbp - receptions)

"

# Step: receiver charting summary

recv_df <- pbp |> 
  filter(!is.na(epa), !is.na(posteam), pass == 1, two_point_attempt != 1, play_type != 'no_play') |>
  filter(!is.na(receiver_player_id)) |> 
  group_by(posteam, week, receiver_player_id, receiver_player_name) |> 
  summarize(targets_pbp = n(), 
            receptions_pbp = sum(complete_pass), 
            catchable_contested = sum(is_catchable_ball * is_contested_ball), 
            catchable_not_contested = sum(is_catchable_ball * !is_contested_ball),
            not_catchable = sum(!is_catchable_ball),
            created_receptions = sum(is_created_reception),
            drops = sum(is_drop),
            total_epa = sum(epa)
            ) |> 
  ungroup() |> 
  left_join(player_stats |> 
              select(player_id:position, headshot_url, week, targets, 
                     receptions:air_yards_share), 
            by = c('receiver_player_id' = 'player_id', 'week')) |> 
  mutate(epa_per_target = receiving_epa / targets_pbp,
         targets_diff = targets_pbp - targets, 
         receptions_diff = receptions_pbp - receptions,
         epa_diff = total_epa - receiving_epa) |> 
  
  select(-player_name, -receiving_fumbles, -receiving_fumbles_lost, -receiving_first_downs, 
         -receiving_2pt_conversions, -racr, -receiver_player_name, -receiving_air_yards,
         -player_name) |> 
  filter(position %in% c("WR", "TE", "RB")) |> 
  select(receiver_player_id, player_display_name, position, headshot_url, posteam, week, 
         targets_pbp, targets, targets_diff, receptions_pbp, receptions, receptions_diff,
         drops, everything())























