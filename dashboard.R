library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)
library(gt)
library(gtExtras)
library(ggthemes)

# Load data with filtering for selected teams
team_df <- read_csv("team_cutoff_analysis.csv") %>%
  filter(fielding_team %in% c('QEA', 'YJD', 'RZQ'))

fielder_df <- read_csv("fielder_eval.csv") %>%
  filter(team %in% c('QEA', 'YJD', 'RZQ')) %>%
  filter(total_actions >= 4) %>%
  group_by(cutoff_id)

all_mistakes <- read_csv('fielder_mistakes_grouped.csv')

stats <- read_csv("fielder_stats.csv") %>%
  filter(fielding_team %in% c('QEA', 'YJD', 'RZQ'))

ui <- dashboardPage(
  dashboardHeader(title = "Cutoff Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Analysis", tabName = "team", icon = icon("users")),
      menuItem("Fielder Analysis", tabName = "fielder", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Team Tab
      tabItem(
        tabName = "team",
        fluidRow(
          box(
            title = "Select Team", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("team_select", "Team:", choices = unique(team_df$fielding_team))
          ),
          valueBoxOutput("team_plays"),
          valueBoxOutput("team_accuracy"),
          valueBoxOutput("team_ev_loss")
        ),
        fluidRow(
          box(
            title = "Most Vulnerable Paths", width = 6, 
            plotOutput("team_accuracy_bar")
          ),
          box(
            title = "Fielder Arm Strengths", width = 6,
            dataTableOutput("arm_strength_table")
          )
          
        )
      ),
      
      # Fielder Tab
      tabItem(
        tabName = "fielder",
        fluidRow(
          box(
            title = "Select Fielder", status = "info", solidHeader = TRUE, width = 4,
            selectInput("fielder_select", "Fielder:", choices = unique(fielder_df$cutoff_id))
          ),
          valueBoxOutput("fielder_plays"),
          valueBoxOutput("fielder_accuracy"),
          valueBoxOutput("fielder_position"),
          valueBoxOutput("fielder_rank")
        ),
        fluidRow(
          box(
            title = "Fielder Percentile Rankings", width = 6, 
            plotOutput("fielder_accuracy_plot")
          ),
          box(
            title = "Most Common Mistakes (Selected Fielder)", width = 6, 
            plotOutput("fielder_mistake_plot")
          )
        ),
        fluidRow(
          box(
            title = "Overall Most Common Mistakes (All Fielders)", width = 12,
            plotOutput("overall_mistake_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  ### TEAM TAB ###
  # TEAM VALUEBOXES
  output$team_plays <- renderValueBox({
    plays <- team_df %>% filter(fielding_team == input$team_select) %>% pull(total_cutoff_plays)
    valueBox(plays, "Total Plays", icon = icon("baseball-ball"), color = "blue")
  })
  
  output$team_accuracy <- renderValueBox({
    acc <- team_df %>% filter(fielding_team == input$team_select) %>% pull(accuracy_rate)
    valueBox(scales::percent(acc), "Optimal Decision Rate", icon = icon("check-circle"), color = "green")
  })
  
  output$team_ev_loss <- renderValueBox({
    loss <- team_df %>% filter(fielding_team == input$team_select) %>% pull(avg_ev_penalty)
    valueBox(round(loss, 3), "Avg Run Value Lost", icon = icon("chart-line"), color = "red")
  })
  
  # TEAM PLOTS with highlighting for selected team:
  output$arm_strength_table <- renderDataTable({
    stats %>%
      filter(fielding_team == input$team_select) %>%
      select(fielder_id, `as+`) %>%
      arrange(desc(`as+`)) %>%
      datatable(
        colnames = c("Fielder", "AS+"),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          ordering = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        "as+",
        backgroundColor = styleInterval(
          cuts = quantile(stats$`as+`, probs = seq(0, 1, length.out = 11), na.rm = TRUE)[-c(1, 11)],
          values = c(
            "#cce5ff", "#99ccff", "#66b3ff", "#3399ff", "#007fff",
            "#0066cc", "#0052a3", "#003d80", "#002952", "#001933"
          )
        ),
        color = "white"
      )
    
  })
  
  
  output$team_accuracy_bar <- renderPlot({
    team_df %>%
      filter(fielding_team == input$team_select) %>%
      select(fielding_team, contains("->")) %>%
      pivot_longer(cols = contains("->"),
                   names_to = "basepath",
                   names_prefix = "ev_loss_",
                   values_to = "ev_loss") %>%
      ggplot(aes(x = reorder(basepath, -ev_loss), y = ev_loss)) +
      geom_col(fill = "#0096FF") +
      labs(
        x = "Base Running Path",
        y = "Avg Run Value Loss",
        title = paste("Avg Run Value Loss by Baserunner Path for Team", input$team_select)
      ) +
      coord_flip() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 11, color = "black")
      ) + 
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  ### FIELDER TAB ###
  # FIELDER VALUEBOXES
  output$fielder_plays <- renderValueBox({
    plays <- fielder_df %>% filter(cutoff_id == input$fielder_select) %>% pull(total_actions) %>% first()
    valueBox(plays, "Total Plays", icon = icon("person-running"), color = "blue")
  })
  
  output$fielder_accuracy <- renderValueBox({
    acc <- fielder_df %>% filter(cutoff_id == input$fielder_select) %>% pull(accuracy) %>% first()
    valueBox(scales::percent(acc), "Optimal Decision Rate", icon = icon("check-circle"), color = "green")
  })
  
  output$fielder_position <- renderValueBox({
    pos <- fielder_df %>% 
      filter(cutoff_id == input$fielder_select) %>% 
      pull(position) %>% 
      unique()
    
    pos <- if (length(pos) > 0) stringr::str_to_title(pos) else NA
    
    valueBox(pos, "Position", icon = icon("id-badge"), color = "yellow")
  })
  
  output$fielder_rank <- renderValueBox({
    fielder_ranks <- fielder_df %>%
      ungroup() %>%
      arrange(desc(accuracy)) %>%
      mutate(rank = row_number())
    
    selected_rank <- fielder_ranks %>%
      filter(cutoff_id == input$fielder_select) %>%
      pull(rank)
    
    valueBox(selected_rank, "Accuracy Rank (out of 28)", icon = icon("sort-numeric-up"), color = "aqua")
  })
  
  
  output$fielder_accuracy_plot <- renderPlot({
    library(ggplot2)
    library(dplyr)
    library(forcats)
    
    # Compute percentiles
    percentiles <- fielder_df %>%
      ungroup() %>%
      mutate(
        acc_pct = percent_rank(accuracy),
        ev_lost_pct = 1 - percent_rank(ev_lost)
      ) %>%
      filter(cutoff_id == input$fielder_select) %>%
      select(acc_pct, ev_lost_pct) %>%
      pivot_longer(cols = everything(), names_to = "metric", values_to = "percentile") %>%
      mutate(
        metric = recode(metric,
                        acc_pct = "Optimal Decision Rate",
                        ev_lost_pct = "Run Value Saved"),
        percentile = round(percentile * 100),
        bar_fill = case_when(
          percentile >= 75 ~ "#d7191c",  # Great - red
          percentile >= 40 ~ "#abd9e9",  # Average - light blue
          TRUE ~ "#2c7bb6"              # Poor - dark blue
        )
      )
    
    ggplot(percentiles, aes(x = percentile, y = fct_rev(metric))) +
      # Lollipop line
      geom_segment(aes(x = 0, xend = percentile, y = metric, yend = metric),
                   color = "gray70", size = 10) +
      # Circle at end
      geom_point(aes(color = bar_fill), size = 20) +
      geom_text(aes(label = percentile), color = "white", fontface = "bold", size = 6) +
      # POOR, AVERAGE, GREAT axis labels
      scale_x_continuous(
        limits = c(0, 100),
        breaks = c(0, 50, 100),
        labels = c("POOR", "AVERAGE", "GREAT")
      ) +
      scale_color_identity() +
      theme_minimal(base_size = 14) +
      labs(x = "", y = "") +
      theme(
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold",
                                   color = c("blue", "gray40", "red")),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "gray85"),
        plot.margin = margin(10, 20, 10, 10)
      )
  })
  
  
  
  
  # FIELDER MISTAKES: Show most common mistakes for selected fielder.
  output$fielder_mistake_plot <- renderPlot({
    fielder_mistakes <- all_mistakes %>%
      filter(cutoff_id == input$fielder_select, !is.na(mistake_type))
    
    ggplot(fielder_mistakes, aes(x = reorder(mistake_type, count), y = count)) +
      geom_col(fill = "#0096FF") +
      coord_flip() +
      labs(x = "Mistake Type", y = "Count", title = paste("Mistakes for", input$fielder_select)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 11, color = "black")
      ) + 
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # Overall mistakes across all fielders
  output$overall_mistake_plot <- renderPlot({
    overall <- all_mistakes %>%
      filter(!is.na(mistake_type)) %>%
      group_by(mistake_type) %>%
      summarize(total_count = sum(count, na.rm = TRUE)) %>%
      arrange(desc(total_count)) %>%
      slice_head(n = 10)
    
    ggplot(overall, aes(x = reorder(mistake_type, total_count), y = total_count)) +
      geom_col(fill = "#d4af37") +
      coord_flip() +
      labs(x = "Mistake Type", y = "Total Count", title = "") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 11, color = "black")
      ) + theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
}

shinyApp(ui, server)
