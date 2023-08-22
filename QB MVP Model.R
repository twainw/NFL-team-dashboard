library(tidyverse)
library(nflreadr)
library(rvest)
library(yardstick)
library(gt)
library(gtExtras)
library(gtsummary)
`%nin%` = Negate(`%in%`)

#--------------------------------------------------------
# Prepare Model Data #
#--------------------------------------------------------

#--------------------------
# QB Player Stats
#--------------------------

player_stats <- nflreadr::load_player_stats(2000:season_yr) %>%
  filter(season_type == "REG" & position == "QB") %>%
  filter(season != 2000 & season != 2005 & season != 2006 & season != 2012) %>%
  group_by(season, team = recent_team, player_display_name, player_id) %>%
  summarize(
    total_cmp = sum(completions, na.rm = TRUE),
    total_attempts = sum(attempts, na.rm = TRUE),
    total_yards = sum(passing_yards + rushing_yards, na.rm = TRUE),
    total_tds = sum(passing_tds + rushing_tds, na.rm = TRUE),
    total_interceptions = sum(interceptions, na.rm = TRUE),
    mean_epa = mean(passing_epa, na.rm = TRUE)) %>%
  filter(total_attempts >= 150) %>%
  ungroup() |> 
  filter(season >= 2003)

#--------------------------
# Standings
#--------------------------

standings <- readr::read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/standings.csv") %>% 
  mutate(team = case_when(
    team == 'OAK' ~ 'LV',
    team == 'SD' ~ 'LAC',
    team == 'STL' ~ 'LA',
    TRUE ~ team)) |> 
  filter(season != 2023) |> 
  filter(season >= 2003)

#--------------------------
# Compute Ranks and pull standings
#--------------------------

qb_mvp_stats <- player_stats %>%
  dplyr::group_by(season) %>%
  mutate(cmp_rank = order(order(total_cmp, decreasing = TRUE)),
         att_rank = order(order(total_attempts, decreasing = TRUE)),
         yds_rank = order(order(total_yards, decreasing = TRUE)),
         tds_rank = order(order(total_tds, decreasing = TRUE)),
         int_rank = order(order(total_interceptions, decreasing = FALSE)),
         epa_rank = order(order(mean_epa, decreasing = TRUE))) %>%
  select(season, player_display_name, player_id, cmp_rank, att_rank, yds_rank, tds_rank,
         int_rank, epa_rank, team, total_interceptions) |> 
  left_join(standings %>% select(season, team, wins), by = c('team', 'season')) %>% 
  mutate(win_rank = rank(desc(wins), ties.method = 'average'))

#--------------------------
# MVP Winners
#--------------------------

link = "https://www.pro-football-reference.com/awards/ap-nfl-mvp-award.htm"
content <- read_html(link)
tables <- html_table(content)
mvps <- tables[[1]]

mvps_final <- mvps %>% 
  mutate(MVP = 1) %>% 
  mutate(player_display_name = Player, season = Year, position = Pos, mvp = MVP) %>% 
  select(player_display_name, position, mvp, season) |> 
  filter(season >= 2003)

#--------------------------
# Join stats and response 
#--------------------------

qb_mvp_stats_final <- qb_mvp_stats %>%
  left_join(mvps_final, by = c("season", "player_display_name")) |> 
  select(-position) |> 
  replace_na(list(mvp = 0)) %>% 
  arrange(season, player_display_name) |> 
  ungroup() |> 
  relocate(team, .after = player_id)

#--------------------------
# Sanity Checks
#--------------------------

# Step: Any missing values?

qb_mvp_stats_final %>% 
  mutate(num_missing = sum(is.na(c_across(cmp_rank:last_col())))) %>% 
  summarise(sum(num_missing))

# Step: Check the MVP Column

table(qb_mvp_stats_final$mvp)

#--------------------------
# Training and Testing sets
#--------------------------

df_train <- qb_mvp_stats_final %>% 
  filter(season >= 2003 & season <= 2018 & season %nin% c(2005, 2006, 2012))

df_test <- qb_mvp_stats_final %>% filter(season >= 2019)

# Step: double check the training and testing sets

table(df_train$season)
table(df_test$season)

#--------------------------------------------------------
# Modeling #
#--------------------------------------------------------

m <- glm(formula = mvp ~ tds_rank + yds_rank + win_rank + epa_rank + total_interceptions,
         data = df_train, family = binomial)

# Step: Review coefficients

summary(m)

#--------------------------
# Quantifying training predictions
#--------------------------

options(scipen = 99)

# Step: Normalize Probabilities

df_train <- df_train %>% 
  mutate(pred = predict(m, df_train, type = "response")) %>% 
  group_by(season) %>%
  mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
  mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
  ungroup()

# Step: Training prediction's confusion matrix

confusion_train <- conf_mat(table(df_train$pred_mvp, df_train$mvp))
summary(confusion_train, event_level = 'second')

# Step: Find incorrectly predicted training seasons

df_train %>% 
  filter((mvp == 1 & pred_mvp == 0) | (mvp == 0 & pred_mvp == 1)) %>% 
  mutate(outcome = case_when(mvp == 1 & pred_mvp == 0 ~ "Actual MVP", TRUE ~ "Predicted MVP")) %>% 
  select(season, player_display_name, outcome) %>% 
  pivot_wider(names_from = outcome, values_from = player_display_name)

#--------------------------
# Quantifying testing predictions
#--------------------------

# Step: Normalize Probabilities

df_test <- df_test %>% 
  mutate(pred = predict(m, df_test, type = "response")) %>% 
  group_by(season) %>%
  mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
  mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
  ungroup()

# Step: Testing prediction's confusion matrix

confusion_test <- conf_mat(table(df_test$pred_mvp, df_test$mvp))
summary(confusion_test, event_level = 'second')

# Step: Find incorrectly predicted training seasons

df_test %>% 
  filter((mvp == 1 & pred_mvp == 0) | (mvp == 0 & pred_mvp == 1)) %>% 
  mutate(outcome = case_when(mvp == 1 & pred_mvp == 0 ~ "Actual MVP", TRUE ~ "Predicted MVP")) %>% 
  select(season, player_display_name, outcome) %>% 
  pivot_wider(names_from = outcome, values_from = player_display_name)

#--------------------------------------------------------
# View Model Results #
#--------------------------------------------------------

# Step: Define columns to display in the gt() table

cols_needed <- c("player_display_name", "mvp_prob", "pred_mvp", "mvp", 
                 "tds_rank", "yds_rank", "win_rank", "epa_rank", "total_interceptions")

# Step: Create gt() table function

get_gt_table <- function(df, year, num_rows = 5) {
  
  df %>% 
    filter(season == year) %>% 
    arrange(season, -mvp_prob) %>% 
    filter(row_number() <= num_rows) %>% 
    select(all_of(cols_needed)) %>% 
    gt() %>% 
    
    # add a title
    tab_header(title = paste0(year, " MVP Race")) %>%
    
    # change column labels
    cols_label(
      player_display_name = "Name",
      mvp_prob = md("MVP<br>Prob"),
      pred_mvp = md("Predicted<br>MVP"),
      mvp = md("Actual<br>MVP"),
      tds_rank = md("Total<br>TDs<br>Rank"),
      yds_rank = md("Total<br>YDs<br>Rank"),
      win_rank = md("Total<br>WINs<br>Rank"),
      epa_rank = md("EPA<br>Rank"),
      total_interceptions = md("Total<br>INTs")
    ) %>% 
    fmt_percent(columns = mvp_prob, decimals = 1) %>% 
    
    # add data color to change
    data_color(
      columns = mvp_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "RColorBrewer::Blues"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>% 
    
    cols_align(align = "center", columns = 3:8) %>% 
    
    # finishing touches
    tab_source_note(source_note = md("**Source**: nflverse | **Note:** Brill & Weisman's MVP Model")) %>% 
    gtExtras::gt_theme_538()
}

get_gt_table(df_test, season_yr)

coeffs <- m |> 
  tbl_regression() |> 
  as_gt() |> 
  tab_header(title = md("**QB MVP Model Coefficients**")) |> 
  gt_theme_538()

coeffs
