library(shiny)
library(nflreadr)
library(tidyverse)
library(nflplotR)
library(gt)
library(gtExtras)
library(shinythemes)
library(shinyWidgets)

#--------------------------------------------------------
# UI #
#--------------------------------------------------------

ui <- fluidPage(
  
  #--------------------------
  # Team Summary - epa by week
  #--------------------------
  
  # Theme
  
  theme = shinytheme("journal"),
  
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
  
  navbarPage("",
             tabPanel("Team Summary",
                      tabsetPanel(
                        tabPanel("Offense", tableOutput("off_eff_summary"), gt_output("off_epa_per_play_by_week")),
                        tabPanel("Defense", gt_output("def_eff_summary"), gt_output("def_epa_per_play_by_week"))
                      )),
             tabPanel("MVP Tracker"),
             tabPanel("QB Stats")
  )
)

#--------------------------------------------------------
# Server #
#--------------------------------------------------------

server <- function(input, output) {
  
  #--------------------------
  # Team Summary - epa by week
  #--------------------------
  
  wk_table <- function(df, reverse_arg){
    
    data <- df |> 
      filter(team == input$team,
             week %in% input$week[1]:input$week[2])
    
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
      data_color(
        columns = c(dropback_epa, rush_epa, epa_per_play),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_c(
            palette = "ggthemes::Orange-Blue Light Diverging",
            n = 10,
            direction = 1
          ) |>  as.character(),
          domain = c(-1, 1), 
          reverse = reverse_arg,
          na.color = "#00441BFF"
        )
      ) |> 
      data_color(
        columns = c(dropback_sr, rush_sr, sr),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_c(
            palette = "ggthemes::Orange-Blue Light Diverging",
            n = 10,
            direction = 1
          ) |>  as.character(),
          domain = c(0, 1), 
          reverse = reverse_arg,
          na.color = "#00441BFF"
        )
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
        footnote = md("Passes where yards gained >= 20"),
        locations = cells_column_labels(columns = "dropback_explosive")
      ) |> 
      tab_footnote(
        footnote = md("Passes where yards gained >= 15"),
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
        title = team_html()) |> 
      gt_theme_538()
  }
  
  # Offense
  
  output$off_epa_per_play_by_week <- render_gt({
    wk_table(off_stats_df, reverse_arg = "F")
  })
  
  # Defense
  
  output$def_epa_per_play_by_week <- render_gt({
    wk_table(def_stats_df, reverse_arg = "T")
  })
  
  #--------------------------
  # Effectiveness Summary
  #--------------------------
  
  eff_table <- function(df){
    
    data <- df |> 
      filter(week %in% input$week[1]:input$week[2]) |> 
      group_by(season, team, type, team_logo) |> 
      summarize(epa_per_play = sum(total_epa) / sum(total_plays), 
                sr = sum(total_success) / sum(total_plays), 
                dropback_epa = sum(total_pass_epa) / sum(total_passes), 
                dropback_sr = sum(total_pass_success) / sum(total_passes), 
                rush_epa = sum(total_rush_epa) / sum(total_rushes), 
                rush_sr = sum(total_rush_success) / sum(total_rushes),
                pts = sum(pts)
      ) |> 
      ungroup() |> 
      mutate(across(epa_per_play:pts,
                    ~ ifelse(type == "offense", rank(-.x, ties.method = 'min'), rank(.x, ties.method = 'min')),
                    .names = "{col}_rank")) |> 
      filter(team == input$team) |> 
      select(season, team_logo, epa_per_play:last_col())
      
    data |> gt() |> 
      fmt_number(columns = c("dropback_epa", "rush_epa", "epa_per_play"), decimals = 2) |> 
      fmt_percent(columns = c("sr", "dropback_sr", "rush_sr"), decimals = 1) |> 
      cols_align(align = 'center', columns = c("team_logo")) |> 
      gt_img_rows(team_logo, height = 35) |>
      gt_merge_stack(dropback_epa, dropback_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(dropback_sr, dropback_sr_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_epa, rush_epa_rank,
                     font_weight = c("normal", "normal"),
                     palette = c("black", "black")) |>
      gt_merge_stack(rush_sr, rush_sr_rank,
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
                 rush_epa = md("Rush<br>EPA/Play"),
                 rush_sr = md("Rush<br>Success%"),
                 pts = md("Pts")
      ) |> 
      tab_header(title = md("**Effectiveness Summary**"),
                 ) |>
      tab_footnote(
        footnote = paste0("Subscripted Number: Rank in that category from Weeks ", 
                          input$week[1], "-", input$week[2]),
        locations = cells_column_labels(columns = "season")
      ) |> 
      gt_theme_538() |> 
      opt_align_table_header(align = "center")
  }
  
  # Offense
  
  output$off_eff_summary <- render_gt({
    eff_table(off_eff_df)
  })
  
  # Defense
  
  output$def_eff_summary <- render_gt({
    eff_table(def_eff_df)
  })
}

#--------------------------------------------------------
# Run application #
#--------------------------------------------------------

shinyApp(ui = ui, server = server)
