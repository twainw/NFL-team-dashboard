library(shiny)
library(nflreadr)
library(tidyverse)
library(nflplotR)
library(gt)
library(gtExtras)
library(shinythemes)

#--------------------------------------------------------
# UI #
#--------------------------------------------------------

ui <- fluidPage(
  
  #--------------------------
  # Team Summary - epa by week
  #--------------------------
  
  # Theme
  
  theme = shinytheme("journal"),
  
  # Title
  
  titlePanel("Team Summary"),
  
  # Inputs and plots: sidebarLayout
  
  sidebarLayout(
    
    ## sidebarPanel for inputs: Team and Season Type
    sidebarPanel(
      selectInput("team", "Select Team", 
                  choices = teams |> pull(team_abbr), 
                  selected = "SF",
                  width="120px"),
      width = 2), ## END: sidebarPanel
    
    ## mainPanel for plot: epa/play by week
    mainPanel(
      
      ### tabsetPanel: Offense and Defense
      tabsetPanel(
        tabPanel("Offense", gt_output("off_epa_per_play_by_week")),
        tabPanel("Defense", gt_output("def_epa_per_play_by_week"))
      ) ### END: tabsetPanel
      
    ) ## END: mainPanel
  ) # END: sidebarLayout
) # END: UI

#--------------------------------------------------------
# Server #
#--------------------------------------------------------

server <- function(input, output) {
  
  #--------------------------
  # Team Summary - epa by week
  #--------------------------
  
  table <- function(df){
    
    data <- df |> 
      filter(team == input$team)
    
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
             rushes, rushing_yds, lost_fumbles, rush_epa, rush_epa_rank, 
             rush_sr, rush_sr_rank, tot_plays, tot_yds, tot_tos, epa_per_play,
             epa_per_play_rank, sr, sr_rank) |> 
      gt() |> 
      gt_merge_stack(result, final_score, 
                     font_weight = c("bold", "normal")) |> 
      gt_merge_stack(dropback_epa, dropback_epa_rank,
                     font_weight = c("normal", "normal")) |>
      gt_merge_stack(dropback_sr, dropback_sr_rank,
                     font_weight = c("normal", "normal")) |>
      gt_merge_stack(rush_epa, rush_epa_rank,
                     font_weight = c("normal", "normal")) |>
      gt_merge_stack(rush_sr, rush_sr_rank,
                     font_weight = c("normal", "normal")) |>
      gt_merge_stack(epa_per_play, epa_per_play_rank,
                     font_weight = c("normal", "normal")) |>
      gt_merge_stack(sr, sr_rank,
                     font_weight = c("normal", "normal")) |>
      cols_align(align = 'center', columns = c("week")) |> 
      cols_align(align = 'left', columns = c("final_score", "result")) |> 
      cols_align(align = 'right', columns = c("opp_logo")) |> 
      fmt_percent(columns = c("sr", "dropback_sr", "rush_sr"), decimals = 1) |> 
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
                 rushes = md("Rushes"),
                 rushing_yds = md("Rush<br>Yds"),
                 lost_fumbles = md("Lost<br>Fumbles"),
                 rush_epa = md("EPA/<br>Play"),
                 rush_sr = md("Success<br>%"),
                 tot_plays = md("Plays"),
                 tot_yds = md("Yds"),
                 tot_tos = md("TOs"),
                 epa_per_play = md("EPA/<br>Play"),
                 sr = md("Success<br>%")
      ) |> 
      tab_spanner(label = md("**Spread & Rest Time**"),
                  columns = spread_line:opponent_rest) |> 
      tab_spanner(label = md("**Passing**"),
                  columns = passes:dropback_sr) |> 
      tab_spanner(label = md("**Rushing**"), 
                  columns = rushes:rush_sr) |> 
      tab_spanner(label = md("**Total**"),
                  columns = tot_plays:sr) |> 
      gt_img_rows(opp_logo, height = 35) |> 
      text_transform(locations = cells_body(c(result)),
                     fn = function(x){
                       x <- gsub("W", "<span style=\"color: green;\">W</span>", x)
                       x <- gsub("L", "<span style=\"color: red;\">L</span>", x)
                     }) |> 
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(columns = week)) |> 
      tab_header(
        title = team_html()) |> 
      gt_theme_538()
  }
  
  # Offense
  
  output$off_epa_per_play_by_week <- render_gt({
    table(off_stats_df)
  })
  
  # Defense
  
  output$def_epa_per_play_by_week <- render_gt({
    table(def_stats_df)
  })
  
}

#--------------------------------------------------------
# Run application #
#--------------------------------------------------------

shinyApp(ui = ui, server = server)
