#--------------------------
# PVT table Summary for a team and wk
#--------------------------

pbp |> 
  filter(!is.na(epa), !is.na(posteam), pass == 1 | rush == 1) |> 
  mutate(explosive_pass = ifelse(pass == 1 & yards_gained >= 20, 1, 0),
         explosive_run = ifelse(rush == 1 & yards_gained >= 10, 1, 0)) |>
  drop_na(offense_formation) |> 
  filter(posteam == "SF", week == 4) |> 
  group_by(posteam, week, offense_personnel) |> 
  summarize(epa_per_play = sum(epa) / n()
            , passes = sum(pass)
            , passing_yds = sum(passing_yards, na.rm = T)
            , dropback_epa = sum(epa*pass) / sum(pass)
            , dropback_sr = sum(success*pass) / sum(pass)
            , dropback_explosive = sum(explosive_pass, na.rm = T) / sum(pass)
            , rushes = sum(rush)
            , rushing_yds = sum(rushing_yards, na.rm = T)
            , rush_epa = sum(epa*rush) / sum(rush)
            , rush_sr = sum(success*rush) / sum(rush)
            , rush_explosive = sum(explosive_run, na.rm = T) / sum(rush)
            , sr = sum(success) / n()
            , tot_plays = sum(pass) + sum(rush)
            , tot_yds = sum(passing_yards, na.rm = T) + sum(rushing_yards, na.rm = T))|> 
  ungroup() |> 
  mutate(across(everything(), ~replace(., is.nan(.), 0))) |> 
  select(-posteam, -week) |> 
  select(offense_personnel, 
         tot_plays, tot_yds, epa_per_play, sr,
         passes, passing_yds, dropback_epa, dropback_sr, dropback_explosive,
         rushes, rushing_yds, rush_epa, rush_sr, rush_explosive) |> 
  gt() |> 
  fmt_percent(columns = c("sr", "dropback_sr", "rush_sr", 
                          "dropback_explosive", "rush_explosive"), 
              decimals = 1) |> 
  fmt_number(columns = c("dropback_epa", "rush_epa", "epa_per_play"), decimals = 2) |> 
  cols_label(passes = md("Passes"),
             passing_yds = md("Pass<br>Yds"),
             dropback_epa = md("EPA/<br>Play"),
             dropback_sr = md("Success<br>%"),
             dropback_explosive = md("Explosive<br>%"),
             rushes = md("Rushes"),
             rushing_yds = md("Rush<br>Yds"),
             rush_epa = md("EPA/<br>Play"),
             rush_sr = md("Success<br>%"),
             rush_explosive = md("Explosive<br>%"),
             tot_plays = md("Plays"),
             tot_yds = md("Yds"),
             epa_per_play = md("EPA/<br>Play"),
             sr = md("Success<br>%")
  ) |> 
  tab_spanner(label = md("**Passing**"),
              columns = passes:dropback_explosive) |> 
  tab_spanner(label = md("**Rushing**"), 
              columns = rushes:rush_explosive) |> 
  tab_spanner(label = md("**Total**"),
              columns = tot_plays:sr) |> 
  tab_footnote(
    footnote = md("Passing yards gained >= 20"),
    locations = cells_column_labels(columns = "dropback_explosive")
  ) |> 
  tab_footnote(
    footnote = md("Rush yards gained >= 10"),
    locations = cells_column_labels(columns = "rush_explosive")
  ) |> 
  gt_theme_538() |> 
  opt_align_table_header(align = "center")
