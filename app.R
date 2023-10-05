library(shiny)
library(nflreadr)
library(tidyverse)
library(nflplotR)
library(gt)
library(gtExtras)
library(shinythemes)
library(shinyWidgets)
library(nflfastR)

#--------------------------------------------------------
# Source Programs and define macros #
#--------------------------------------------------------

season_yr <- 2023

#--------------------------------------------------------
# UI #
#--------------------------------------------------------

ui <- fluidPage(
  
  # CSS tags to make verbatimTextOutput look nice
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Chivo&display=swap"),
    tags$style(HTML("
      #explanation {
        background-color: white;
        font-family: 'Chivo', sans-serif;
        font-size: 16px;
      }
    "))
  ),
  
  # Theme
  
  theme = shinytheme("journal"),
  
  navbarPage("",
             
             #--------------------------
             # Offense Tab Panel
             #--------------------------
             
             tabPanel("Offense",
                      selectInput("team", "Team:", 
                                  choices = teams |> pull(team_abbr), 
                                  selected = "SF",
                                  width="120px"),
                      
                      sliderInput("week", "Week:",
                                  (off_stats_df |> distinct(week) |> pull()), 
                                  value = c(off_stats_df |> distinct(week) |> pull() |> min(), 
                                            off_stats_df |> distinct(week) |> pull() |> max()),
                                  min = off_stats_df |> distinct(week) |> pull() |> min(),
                                  max = off_stats_df |> distinct(week) |> pull() |> max()
                      ),
                      tabsetPanel(
                        
                        # Tab Panel: Offensive Summary
                        tabPanel("Offensive Summary",
                                 gt_output("off_eff_summary"), 
                                 gt_output("off_epa_per_play_by_week")),
                        
                        # Tab Panel: Waterfall
                        tabPanel("Receiving Waterfall",
                                 gt_output("recv_table")),
                        
                        # Tab Panel: PVT 
                        tabPanel("PVT",
                                 textInput(inputId = "group_by_list",
                                           label = "List Dimension/Dimensions",
                                           value = "offense_personnel"),
                                 gt_output("off_pvt_table")
                        )
                      )),
             
             #--------------------------
             # Defense Tab Panel
             #--------------------------
             
             tabPanel("Defense",
                      selectInput("def_team", "Team:", 
                                  choices = teams |> pull(team_abbr), 
                                  selected = "SF",
                                  width="120px"),
                      
                      sliderInput("def_week", "Week:",
                                  (def_stats_df |> distinct(week) |> pull()), 
                                  value = c(def_stats_df |> distinct(week) |> pull() |> min(), 
                                            def_stats_df |> distinct(week) |> pull() |> max()),
                                  min = def_stats_df |> distinct(week) |> pull() |> min(),
                                  max = def_stats_df |> distinct(week) |> pull() |> max()
                      ),
                      tabsetPanel(
                        tabPanel("Defensive Summary",
                                 gt_output("def_eff_summary"), 
                                 gt_output("def_epa_per_play_by_week"))
                      )),
             
             #--------------------------
             # QB MVP Tracker Tab Panel
             #--------------------------
             
             tabPanel("QB MVP Tracker", gt_output("mvp_race"), gt_output("coeff"), 
                      h3("Explanation of Coefficients"),
                      verbatimTextOutput("explanation"))
  ))


#--------------------------------------------------------
# Server #
#--------------------------------------------------------

server <- function(input, output) {
  
  #--------------------------
  # Team Summary - epa by week
  #--------------------------
  
  wk_table <- function(df, reverse_arg, team_input, week_input){
    
    data <- df |> 
      filter(team == team_input,
             week %in% week_input[1]:week_input[2])
    
    team_html <- reactive({
      md(
        paste0(
          "<img src=", 
          data$team_logo[1], 
          " style='height:60px; display: block; margin: auto;'>"
        )
      )
    })
    
    data |> 
      select(week, opp_logo, result, final_score, spread_line,
             team_rest, opponent_rest, passes, passing_yds, interceptions, 
             dropback_epa, dropback_epa_rank, dropback_sr, dropback_sr_rank,
             dropback_explosive, dropback_explosive_rank,
             rushes, rushing_yds, lost_fumbles, rush_epa, rush_epa_rank, 
             rush_sr, rush_explosive, rush_explosive_rank,
             rush_sr_rank, tot_plays, tot_yds, tot_tos, epa_per_play,
             epa_per_play_rank, sr, sr_rank) |> 
      gt() |> 
      gt_merge_stack(result, final_score, 
                     font_weight = c("bold", "normal"),
                     palette = c("black", "black")) |> 
      gt_merge_stack(dropback_epa, dropback_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(dropback_sr, dropback_sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(dropback_explosive, dropback_explosive_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_epa, rush_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_sr, rush_sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_explosive, rush_explosive_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(epa_per_play, epa_per_play_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(sr, sr_rank,
                     font_weight = c("normal", "normal")) |>
      cols_align(align = 'center', columns = c("week")) |> 
      cols_align(align = 'left', columns = c("final_score", "result")) |> 
      cols_align(align = 'right', columns = c("opp_logo")) |> 
      fmt_percent(columns = c("sr", "dropback_sr", "rush_sr", 
                              "dropback_explosive", "rush_explosive"), 
                  decimals = 1) |> 
      fmt_number(columns = c("dropback_epa", "rush_epa", "epa_per_play"), decimals = 2) |> 
      cols_label(week = "Wk.", 
                 opp_logo = "", 
                 result = "Result",
                 spread_line = md("Spread"),
                 team_rest = md("Team<br>Rest"),
                 opponent_rest = md("Opp<br>Rest"),
                 passes = md("Passes"),
                 passing_yds = md("Pass<br>Yds"),
                 interceptions = md("INTs"),
                 dropback_epa = md("EPA/<br>Play"),
                 dropback_sr = md("Success<br>%"),
                 dropback_explosive = md("Explosive<br>%"),
                 rushes = md("Rushes"),
                 rushing_yds = md("Rush<br>Yds"),
                 lost_fumbles = md("Lost<br>Fumbles"),
                 rush_epa = md("EPA/<br>Play"),
                 rush_sr = md("Success<br>%"),
                 rush_explosive = md("Explosive<br>%"),
                 tot_plays = md("Plays"),
                 tot_yds = md("Yds"),
                 tot_tos = md("Total<br>TOs"),
                 epa_per_play = md("EPA/<br>Play"),
                 sr = md("Success<br>%")
      ) |> 
      tab_spanner(label = md("**Spread & Rest Time**"),
                  columns = spread_line:opponent_rest) |> 
      tab_spanner(label = md("**Passing**"),
                  columns = passes:dropback_explosive) |> 
      tab_spanner(label = md("**Rushing**"), 
                  columns = rushes:rush_explosive) |> 
      tab_spanner(label = md("**Total**"),
                  columns = tot_plays:sr) |> 
      gt_img_rows(opp_logo, height = 35) |> 
      text_transform(locations = cells_body(c(result)),
                     fn = function(x){
                       x <- gsub("W", "<span style=\"color: green;\">W</span>", x)
                       x <- gsub("L", "<span style=\"color: red;\">L</span>", x)
                     }) |> 
      tab_footnote(
        footnote = md("Passing yards gained >= 20"),
        locations = cells_column_labels(columns = "dropback_explosive")
      ) |> 
      tab_footnote(
        footnote = md("Rush yards gained >= 10"),
        locations = cells_column_labels(columns = "rush_explosive")
      ) |> 
      tab_footnote(
        footnote = md("Subscripted Number: Week's rank in that category"),
        locations = cells_column_labels(columns = "week")
      ) |> 
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(columns = week)) |> 
      tab_header(
        title = md("**By Week Summary**"), 
        subtitle = team_html()) |> 
      gt_theme_538() |> 
      opt_align_table_header(align = "center")
  }
  
  # Offense
  
  output$off_epa_per_play_by_week <- render_gt({
    wk_table(off_stats_df, reverse_arg = "F", input$team, input$week)
  })
  
  # Defense
  
  output$def_epa_per_play_by_week <- render_gt({
    wk_table(def_stats_df, reverse_arg = "T", input$def_team, input$def_week)
  })
  
  #--------------------------
  # Effectiveness Summary
  #--------------------------
  
  eff_table <- function(df, team_input, week_input){
    
    data <- df |> 
      filter(week %in% week_input[1]:week_input[2]) |> 
      group_by(season, team, type, team_logo) |> 
      summarize(epa_per_play = sum(total_epa) / sum(total_plays), 
                sr = sum(total_success) / sum(total_plays), 
                dropback_epa = sum(total_pass_epa) / sum(total_passes), 
                dropback_sr = sum(total_pass_success) / sum(total_passes), 
                dropback_exp = sum(total_pass_explosive) / sum(total_passes),
                rush_epa = sum(total_rush_epa) / sum(total_rushes), 
                rush_sr = sum(total_rush_success) / sum(total_rushes),
                rush_exp = sum(total_rush_explosive) / sum(total_rushes),
                pts = sum(pts)
      ) |> 
      ungroup() |> 
      mutate(across(epa_per_play:pts,
                    ~ ifelse(type == "offense", rank(-.x, ties.method = 'min'), rank(.x, ties.method = 'min')),
                    .names = "{col}_rank")) |> 
      filter(team == team_input) |> 
      left_join(season_standing |> select(team, wins, losses, ties), by = "team") |> 
      select(season, team_logo, epa_per_play:last_col())
    
    data |> gt() |> 
      fmt_number(columns = c("dropback_epa", "rush_epa", "epa_per_play"), decimals = 2) |> 
      fmt_percent(columns = c("sr", "dropback_sr", "rush_sr", "dropback_exp", "rush_exp"), decimals = 1) |> 
      cols_align(align = 'center', columns = c("team_logo")) |> 
      gt_img_rows(team_logo, height = 35) |>
      gt_merge_stack(dropback_epa, dropback_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(dropback_sr, dropback_sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(dropback_exp, dropback_exp_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_epa, rush_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_sr, rush_sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_exp, rush_exp_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(epa_per_play, epa_per_play_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(sr, sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(pts, pts_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      cols_label(season = "Season",
                 team_logo = "",
                 epa_per_play = md("Overall<br>EPA/Play"),
                 sr = md("Overall<br>Success%"),
                 dropback_epa = md("Pass<br>EPA/Play"),
                 dropback_sr = md("Pass<br>Success%"),
                 dropback_exp = md("Pass<br>Explosive%"),
                 rush_epa = md("Rush<br>EPA/Play"),
                 rush_sr = md("Rush<br>Success%"),
                 rush_exp = md("Rush<br>Explosive%"),
                 pts = md("Pts"),
                 wins = md("Reg<br>Wins"),
                 losses = md("Reg<br>Losses"),
                 ties = md("Reg<br>ties")
      ) |> 
      tab_header(title = md("**Effectiveness Summary**"),
      ) |>
      tab_footnote(
        footnote = paste0("Subscripted Number: Rank in that category from Weeks ", 
                          input$week_input[1], "-", input$week_input[2]),
        locations = cells_column_labels(columns = "season")
      ) |> 
      gt_theme_538() |> 
      opt_align_table_header(align = "center")
  }
  
  # Offense
  
  output$off_eff_summary <- render_gt({
    eff_table(off_eff_df, input$team, input$week)
  })
  
  # Defense
  
  output$def_eff_summary <- render_gt({
    eff_table(def_eff_df, input$def_team, input$def_week)
  })
  
  #--------------------------
  # Receiving Waterfall
  #--------------------------
  
  recv_waterfall_table <- function(df, team_input, week_input){
    
    data <- df |> 
      filter(posteam == team_input, 
             week %in% week_input[1]:week_input[2])
    
    data |> 
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
      gt_theme_538()
  }
  
  output$recv_table <- render_gt({
    recv_waterfall_table(recv_df, input$team, input$week)
  })
  
  #--------------------------
  # MVP Race
  #--------------------------
  
  output$mvp_race <- render_gt({
    get_gt_table(df_test, season_yr)
  })
  
  output$coeff <- render_gt({
    m |> 
      tbl_regression() |> 
      as_gt() |> 
      tab_header(title = md("**QB MVP Model Coefficients**")) |> 
      gt_theme_538()
  })
  
  output$explanation <- renderText({
    "Note that coefficients are negative, which indicates having a low rank-value (i.e. 1,2,3) corresponds to a higher rank in that category than ones peers, 
    which is associated with a higher probability of winning the NFL MVP. The negative coefficient of total_interceptions also makes sense, because it indicates 
    throwing more interceptions is associated with a lower probability of winning the NFL MVP."
  })
  
  #--------------------------
  # PVT
  #--------------------------
  
  pvt_table <- function(team_input, week_input, group_by_list){
    
    # Step: Setup group_vars for the group_by function
    
    group_vars <- strsplit(group_by_list, ",")[[1]]
    group_vars <- trimws(group_vars)
    
    # Step: Filter the data
    
    df <- pbp |> 
      filter(!is.na(epa), !is.na(posteam), pass == 1 | rush == 1) |> 
      mutate(explosive_pass = ifelse(pass == 1 & yards_gained >= 20, 1, 0),
             explosive_run = ifelse(rush == 1 & yards_gained >= 10, 1, 0)) |>
      drop_na(offense_formation)
    
    # Step: Create GT() table
    
    df |> 
      filter(week %in% week_input[1]:week_input[2]) |> 
      group_by(posteam, across(all_of(group_vars))) |> 
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
                , tot_yds = sum(passing_yards, na.rm = T) + sum(rushing_yards, na.rm = T)) |> 
      ungroup() |> 
      mutate(across(everything(), ~replace(., is.nan(.), 0))) |> 
      filter(posteam == team_input) |> 
      select(all_of(group_vars), 
             tot_plays, tot_yds, epa_per_play, sr,
             passes, passing_yds, dropback_epa, dropback_sr, dropback_explosive,
             rushes, rushing_yds, rush_epa, rush_sr, rush_explosive) |>
      arrange(-tot_plays) |>  
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
  }
  
  output$off_pvt_table <- render_gt({
    pvt_table(input$team, input$week, input$group_by_list)
  })

}

#--------------------------------------------------------
# Run application #
#--------------------------------------------------------

shinyApp(ui = ui, server = server)
