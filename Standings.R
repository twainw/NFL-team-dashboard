#--------------------------------------------------------
# Preliminaries  #
#--------------------------------------------------------

#--------------------------
# Packages
#--------------------------

library(tidyverse)
library(gt)
library(gtExtras)
library(nflreadr)

#--------------------------
# Data
#--------------------------

# Step: Read Team Futures

team_odds <- readxl::read_xlsx("C:/Users/Ivan Patel/Documents/NFL-team-dashboard/team_futures/NFL Team Futures - latest.xlsx",
                               sheet = "Final Data")

#--------------------------------------------------------
# Data Prep #
#--------------------------------------------------------

#--------------------------
# Add team-related info
  # - team logo, conf, division
  # - standings incl. points info
#--------------------------

final_standings_df <- team_odds |> 
  left_join(teams |> select(team_abbr, team_nick, team_conf, team_division, team_logo_espn), 
            by = c("team" = "team_nick")) |> 
  left_join(season_standing |> select(team, wins, losses, ties, scored, allowed, net), 
            by = c("team_abbr" = "team")) |> 
  select(team_logo_espn, team, team_division, team_conf, wins:net, win_total, 
         division_odds, playoff_odds, sb_odds)

#--------------------------------------------------------
# GT() table #
#--------------------------------------------------------

final_standings_df |> 
  group_by(team_division) |> 
  arrange(team_division, -wins) |> 
  ungroup() |> 
  gt(groupname_col = "team_division") |> 
  gt_img_rows(columns = team_logo_espn) |> 
  gt_theme_538()





