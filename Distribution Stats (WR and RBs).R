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

# Step: Join charting and pbp data

pbp_w_charting <- pbp |> 
  left_join(charting |> select(nflverse_game_id, nflverse_play_id, is_no_huddle:last_col()), 
            by = c("nflverse_game_id", "play_id" = "nflverse_play_id"))

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

recv_df <- pbp_w_charting |> 
  filter(!is.na(epa), !is.na(posteam), pass == 1, two_point_attempt != 1, play_type != 'no_play') |>
  filter(!is.na(receiver_player_id)) |> 
  group_by(posteam, week, receiver_player_id, receiver_player_name) |> 
  summarize(targets_pbp = n(), 
            receptions_pbp = sum(complete_pass), 
            catchable_contested = sum(is_catchable_ball * is_contested_ball), 
            catchable_not_contested = sum(is_catchable_ball * !is_contested_ball),
            not_catchable = sum(!is_catchable_ball),
            created_receptions = sum(is_created_reception),
            drops = sum(is_drop)
            ) |> 
  ungroup() |> 
  left_join(player_stats |> 
              select(player_id:position, headshot_url, week, targets, 
                     receptions:air_yards_share), 
            by = c('receiver_player_id' = 'player_id', 'week')) |> 
  mutate(epa_per_target = receiving_epa / targets_pbp,
         targets_diff = targets_pbp - targets, 
         receptions_diff = receptions_pbp - receptions) |> 
  
  select(-player_name, -receiving_fumbles, -receiving_fumbles_lost, -receiving_first_downs, 
         -receiving_2pt_conversions, -racr, -receiver_player_name, -receiving_air_yards,
         -player_name) |> 
  filter(position %in% c("WR", "TE", "RB")) |> 
  select(receiver_player_id, player_display_name, position, headshot_url, posteam, week, 
         targets_pbp, targets, targets_diff, receptions_pbp, receptions, receptions_diff,
         drops, everything())

# Step: QC the targets and receptions difference

recv_df |> 
  summarize(sum(targets_diff), sum(receptions_diff))

# Step: Create a gt()

recv_df |> 
  filter(posteam == "input$team", week %in% input$week[1]:input$week[2]) |> 
  group_by(headshot_url, player_display_name, position) |> 
  summarize(targets = sum(targets), 
            receptions = sum(receptions), 
            catchable_contested = sum(catchable_contested), 
            catchable_not_contested = sum(catchable_not_contested),
            not_catchable = sum(not_catchable),
            created_receptions = sum(created_receptions),
            drops = sum(drops),
            total_epa = sum(receiving_epa)
            ) |> 
  ungroup() |> 
  mutate(epa_per_target = total_epa / targets) |> 
  arrange(-targets) |> 
  gt() |> 
  fmt_number(columns = c("total_epa", "epa_per_target"), decimals = 2) |> 
  gt_img_rows(headshot_url, height = 35) |> 
  cols_label(
    headshot_url = "",
    player_display_name = "Player",
    position = "Position", 
    targets = "Targets", 
    receptions = "Receptions",
    catchable_contested = md("Catchable<br> but Contested"),
    catchable_not_contested = md("Catchable<br> Not Contested"),
    not_catchable = md("Not<br>Catchable"),
    created_receptions = md("Created<br>Receptions"),
    drops = "Drops",
    total_epa = md("Total<br>EPA"),
    epa_per_target = md("EPA/Target")
  ) |> 
  tab_header(title = md("**Receiving Waterfall**")) |> 
  gt_theme_538()

























