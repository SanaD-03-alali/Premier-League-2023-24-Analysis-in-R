library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(treemapify)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(zoo)
library(scales)
library(leaflet.providers)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f8f9fa;
        color: #2c3e50;
      }
      
      .title {
        color: #2c3e50;
        text-align: center;
        padding: 20px 0;
        font-weight: bold;
        font-size: 24px;
        border-bottom: 2px solid #3498db;
        margin-bottom: 20px;
      }
    "))
  ),
  
  titlePanel("Premier League 2023/24 Analysis"),
  
  # Add tabs
  tabsetPanel(
    # First tab - Original visualization
    tabPanel("Team & Player Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_type", "Select Visualization",
                             choices = c("Goals vs xG" = "scatter",
                                         "Progressive Passes vs Assists" = "passes",
                                         "Player Impact Treemap" = "treemap",
                                         "Formation Impact" = "formation",
                                         "Possession Trends" = "possession",
                                         "Goal Efficiency CI" = "CI")),
                 
                 # Conditional inputs for treemap
                 conditionalPanel(
                   condition = "input.plot_type == 'treemap'",
                   selectInput("impact_metric", "Impact Metric",
                               choices = c("Goals + Assists per 90", "Goals Only", "Assists Only"))
                 ),
                 width = 3
               ),
               
               mainPanel(
                 plotOutput("plot", height = "800px"),
                 uiOutput("explanation"),
                 width = 9
               )
             )
    ),
    
    # Updated second tab
    tabPanel("Stadium Analysis",
             fluidRow(
               column(12,
                      leafletOutput("map", height = "600px"),
                      br(),  # Add some spacing
                      uiOutput("map_explanation")  # New explanation output
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  # Read data
  data <- reactive({
    read.csv("premier-player-23-24.csv")
  })
  
  stadium_data <- reactive({
    read.csv("stadiums-with-GPS-coordinates.csv")
  })
  
  matches_data <- reactive({
    read.csv("matches.csv")
  })
  
  # Render plot based on selection
  output$plot <- renderPlot({
    if(input$plot_type == "scatter") {
      # Goals vs xG scatter plot
      team_xG <- data() %>%
        group_by(Team) %>%
        summarize(
          Total_Goals = sum(Gls, na.rm = TRUE),
          Total_xG = sum(xG, na.rm = TRUE)
        )
      
      # Generate colors based on number of teams
      team_colors <- if (length(unique(team_xG$Team)) <= 12) {
        brewer.pal(n = length(unique(team_xG$Team)), name = "Set3")
      } else {
        colorRampPalette(brewer.pal(12, "Paired"))(length(unique(team_xG$Team)))
      }
      
      ggplot(team_xG, aes(x = Total_xG, y = Total_Goals, color = Team)) +
        geom_point(size = 5, alpha = 0.8) + 
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
        scale_color_manual(values = team_colors) +  
        labs(
          title = "Goals Scored vs. Expected Goals (xG)",
          subtitle = "Analyzing efficiency in converting chances",
          x = "Total Expected Goals (xG)",
          y = "Total Goals Scored",
          color = "Team"
        ) +
        theme_minimal(base_size = 14) +  
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
          plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),  
          axis.title = element_text(face = "bold", size = 16), 
          axis.text = element_text(size = 12),  
          legend.title = element_text(size = 14),  
          legend.text = element_text(size = 12),  
          legend.position = "top",  
          panel.grid.major = element_line(color = "gray90"),  
          panel.grid.minor = element_blank()  
        )
    } else if(input$plot_type == "passes") {
      # Calculate G+A for each player
      data <- data() %>%
        mutate(G_A = Gls + Ast)
      
      # Get top 3 teams based on total G+A
      top_3_teams <- data %>%
        group_by(Team) %>%
        summarize(total_G_A = sum(G_A, na.rm = TRUE)) %>%
        arrange(desc(total_G_A)) %>%
        slice_head(n = 3) %>%
        pull(Team)
      
      # Manually specify bottom 3 teams
      bottom_3_teams <- c("Luton Town", "Sheffield United", "Burnley")
      
      # Combine top and bottom teams
      selected_teams <- c(top_3_teams, bottom_3_teams)
      
      # Get top 3 players for each selected team based on Assists
      top_players <- data %>%
        filter(Team %in% selected_teams) %>%
        group_by(Team) %>%
        arrange(desc(Ast)) %>%
        slice_head(n = 3) %>%
        ungroup() %>%
        distinct(Player, .keep_all = TRUE)
      
      # Create a new factor for team ordering
      top_bottom_order <- c("Liverpool", "Manchester City", "Arsenal", 
                            "Luton Town", "Sheffield United", "Burnley")
      
      top_players$Team <- factor(top_players$Team, levels = top_bottom_order)
      ggplot(top_players, 
             aes(x = PrgP, y = Ast, label = Player)) +
        geom_point(aes(size = Min, color = Team), alpha = 0.7) +
        geom_text_repel(size = 3, max.overlaps = Inf) +
        scale_size_continuous(range = c(2, 8), name = "Total Minutes Played") +
        labs(title = "Top Players: Progressive Passes vs Assists",
             subtitle = "For top and bottom 3 teams",
             x = "Progressive Passes (PrgP)",
             y = "Assists (Ast)") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          legend.text = element_text(size = 14, face = "bold"), 
          legend.title = element_text(size = 16, face = "bold"),
        ) +
        facet_wrap(~ Team, scales = "fixed", nrow = 2) +
        scale_y_continuous(limits = c(0, max(top_players$Ast, na.rm = TRUE) + 5)) +
        scale_x_continuous(limits = c(0, max(top_players$PrgP, na.rm = TRUE) + 5))
    } else if(input$plot_type == "treemap") {
      # Filter for top 4 teams
      top_teams <- c("Manchester City", "Liverpool", "Arsenal", "Aston Villa")
      
      # Calculate impact metrics
      impact_data <- data() %>%
        filter(Team %in% top_teams, Min > 900) %>%
        mutate(
          impact_value = case_when(
            input$impact_metric == "Goals + Assists per 90" ~ (Gls + Ast)/X90s,
            input$impact_metric == "Goals Only" ~ Gls/X90s,
            input$impact_metric == "Assists Only" ~ Ast/X90s
          ),
          total_impact = (Gls * 1 + Ast * 0.8 + PrgP * 0.1 + PrgC * 0.1) / X90s,
          player_label = Player
        )
      
      ggplot(impact_data, 
             aes(area = total_impact, 
                 fill = impact_value,
                 subgroup = Team,
                 label = player_label)) +
        geom_treemap() +
        geom_treemap_subgroup_border(colour = "white", size = 5) +
        geom_treemap_subgroup_text(place = "center", 
                                   grow = TRUE,
                                   alpha = 0.4, 
                                   colour = "white",
                                   fontface = "bold",
                                   min.size = 0) +
        geom_treemap_text(colour = "black", 
                          place = "center",
                          size = 3, 
                          grow = TRUE) +
        scale_fill_gradient2(
          low = "#7FB3D5",    
          mid = "#4A90E2", 
          high = "#1E3C72",
          midpoint = median(impact_data$impact_value),
          name = input$impact_metric
        ) +
        labs(
          title = "Player Impact in Top 4 Teams",
          subtitle = "Box Size = Overall Impact"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.text = element_text(size = 14, color = "black", face = "bold"),
          legend.title = element_text(size = 16, color = "black", face = "bold")
        )
    }
    else if (input$plot_type == "formation") {
      
      # GoalEfficiency: Conversion rate of shots on target to goals
      
      # Filter for specified teams and formations
      formation_impact <- matches %>%
        filter(Team %in% c("ManchesterCity", "Arsenal", "Liverpool", 
                           "AstonVilla", "TottenhamHotspur", "Chelsea")) %>%  # Fixed team names
        mutate(
          Team = case_when(
            Team == "ManchesterCity" ~ "Manchester City",
            Team == "AstonVilla" ~ "Aston Villa",
          Team == "TottenhamHotspur" ~ "Tottenham Hotspur",
            TRUE ~ Team
          ),
          Week = as.numeric(gsub("Matchweek ", "", Round)),
          GoalEfficiency = (GF/SoT * 100)  
        ) %>%
        filter(Formation %in% c("4-2-3-1", "4-3-3", "3-4-3", "3-5-2")) %>%
        arrange(Team, Week)
      
      # Create the visualization with updated legend labels
      ggplot(formation_impact, aes(x = Week)) +
        geom_line(aes(y = Poss, color = "Ball Possession (%)"), size = 0.8) +
        geom_smooth(aes(y = GoalEfficiency, color = "Goal Efficiency (%)"), 
                    method = "loess",    
                    se = FALSE,          
                    span = 0.3,          
                    size = 1) +
        geom_point(aes(y = Poss, shape = Formation), size = 3) +
        facet_wrap(~Team, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = seq(0, 38, by = 5)) +
        scale_color_manual(values = c(
          "Ball Possession (%)" = "blue", 
          "Goal Efficiency (%)" = "red"
        )) +
        theme_minimal() +
        labs(
          title = "Formation Impact on Possession and Goal Efficiency",
          x = "Matchweek",
          y = "Percentage (%)",
          color = "Metrics",
          shape = "Formation"
        ) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(face = "bold"),  
          axis.text = element_text(face = "bold"),   
          legend.position = "right",
          strip.text = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)
        )
    }
    
    
    # For Possession Trends
    else if(input$plot_type == "possession") {
      team_order <- c("Manchester City", "Arsenal", "Liverpool",
                      "Luton Town", "Burnley", "Sheffield United")
      
      possession_trends <- matches_data() %>%
        filter(Team %in% c("ManchesterCity", "Arsenal", "Liverpool", 
                           "LutonTown", "Burnley", "SheffieldUnited")) %>%
        mutate(
          Date = as.Date(Date),
          Month = format(Date, "%b"),
          Points = case_when(
            Result == "W" ~ 3,
            Result == "D" ~ 1,
            Result == "L" ~ 0
          ),
          Team = case_when(
            Team == "ManchesterCity" ~ "Manchester City",
            Team == "LutonTown" ~ "Luton Town",
            Team == "SheffieldUnited" ~ "Sheffield United",
            TRUE ~ Team
          ),
          Team = factor(Team, levels = team_order),
          TeamType = ifelse(Team %in% c("Manchester City", "Arsenal", "Liverpool"), 
                            "Top 3", "Bottom 3")
        ) %>%
        arrange(Team, Date)
      
      # Calculate 4-game moving average
      possession_ma <- possession_trends %>%
        group_by(Team) %>%
        mutate(
          MA4 = rollmean(Poss, k = 4, fill = NA, align = "right")
        )
      
      ggplot(possession_ma, aes(x = Date)) +
        # Add result lines - shorter vertical lines
        geom_segment(aes(
          xend = Date,
          y = Poss - 1,  # Start 1 unit below possession value
          yend = Poss + 1,  # End 1 unit above possession value
          color = Result
        ), size = 1) +  # Adjust size if needed
        # Add 4-game moving average line
        geom_line(aes(y = MA4, linetype = "4-game Moving Average"), 
                  color = "blue", size = 1) +
        facet_wrap(~ Team, ncol = 3, nrow = 2) +
        scale_color_manual(
          name = "Match Result",
          values = c("W" = "#00FF00", "D" = "#FFD700", "L" = "#FF0000"), 
          labels = c("D" = "Draw", "L" = "Loss", "W" = "Win")
        ) +
        scale_linetype_manual(
          name = "Trend Line",
          values = c("4-game Moving Average" = "solid")
        ) +
        labs(
          title = "Possession Trends: Top 3 vs Bottom 3 Teams (2023/24)",
          x = "Date",
          y = "Possession %"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "right",
          legend.box = "vertical",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1, "lines"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold")
        ) +
        scale_y_continuous(limits = c(20, 85)) +
        scale_x_date(date_breaks = "2 month", date_labels = "%b")
    }
    else if (input$plot_type == "CI") {
      
      # Create a vector of teams in the correct order with full names
      team_order <- c("ManchesterCity", "Arsenal", "Liverpool", "AstonVilla", "TottenhamHotspur", 
                      "Chelsea", "NewcastleUnited", "ManchesterUnited", "WestHamUnited", "CrystalPalace",
                      "BrightonandHoveAlbion", "Bournemouth", "Fulham", "WolverhamptonWanderers", "Everton", 
                      "Brentford", "NottinghamForest", "LutonTown", "Burnley", "SheffieldUnited")
      
      # Calculate goal efficiency statistics for all teams
      goal_efficiency <- matches_data() %>%
        # Remove or fix the problematic game
        filter(!(Team == "Brentford" & Date == "2023-12-06")) %>%  # Remove the problematic game
        group_by(Team) %>%
        summarise(
          mean_efficiency = mean(GF/SoT, na.rm = TRUE),
          sd_efficiency = sd(GF/SoT, na.rm = TRUE),
          n = n(),
          se = sd_efficiency / sqrt(n),
          ci_95 = 1.96 * se,
          lower_ci = mean_efficiency - ci_95,
          upper_ci = mean_efficiency + ci_95
        ) %>%
        mutate(Team = factor(Team, levels = rev(team_order)))
      
      # Calculate the mean efficiency across all teams
      mean_eff <- mean(goal_efficiency$mean_efficiency)
      
      # Create the forest plot with average indicator
      ggplot(goal_efficiency, aes(y = Team)) +
        geom_point(aes(x = mean_efficiency), color = "orange", size = 3) +
        geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                      width = 0.2, color = "navy", size = 1) +
        geom_vline(xintercept = mean_eff, 
                   linetype = "dashed", color = "black") +
        # Add text annotation for the mean
        annotate("text", 
                 x = mean_eff, 
                 y = 9.5, # Position above the top team
                 label = sprintf("League Avg: %.3f", mean_eff),
                 size = 5,
                 color = "red",
                 vjust = -0.5,
                 hjust = -0.5) +
        # Add arrow
        annotate("segment",
                 x = mean_eff + 0.018, # Start slightly left of the line
                 xend = mean_eff,     # End at the line
                 y = 9.7,             # Match text y position
                 yend = 9.6,          # Same y position
                 color = "black",
                 arrow = arrow(length = unit(0.3, "cm")),
                 size = 1) +
        labs(
          title = "Goal Efficiency by Team (2023/24 Premier League)",
          subtitle = "Goals per Shot on Target with 95% Confidence Intervals",
          x = "Goals per Shot on Target",
          y = ""
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold", size=12),
          axis.text.x = element_text(face = "bold", size=12),
          panel.grid.minor = element_blank()
        )
      
      
    }
    
    
    
  })
  # Add stadium visualization
  output$map <- renderLeaflet({
    top_10_teams <- c("Manchester City", "Arsenal", "Liverpool", "Aston Villa", 
                      "Tottenham Hotspur", "Chelsea", "Newcastle United", 
                      "Manchester United", "West Ham United", "Crystal Palace")
    
    stadiums <- stadium_data() %>%
      mutate(Team = trimws(Team))
    
    matches_clean <- matches_data() %>%
      mutate(Team = case_when(
        Team == "ManchesterCity" ~ "Manchester City",
        Team == "AstonVilla" ~ "Aston Villa",
        Team == "TottenhamHotspur" ~ "Tottenham Hotspur",
        TRUE ~ Team
      ))
  
  
    venue_performance <- matches_clean %>%
      filter(Team %in% top_10_teams) %>%
      group_by(Team, Venue) %>%
      summarise(
        Games = n(),
        Goals = sum(GF),
        xG_total = sum(xG),
        xG_diff = Goals - xG_total,
        Wins = sum(Result == "W"),
        Draws = sum(Result == "D"),
        Losses = sum(Result == "L"),
        .groups = 'drop'
      )
    
    stadium_effect <- venue_performance %>%
      group_by(Team) %>%
      summarise(
        home_xG_diff = xG_diff[Venue == "Home"],
        away_xG_diff = xG_diff[Venue == "Away"],
        stadium_effect = home_xG_diff - away_xG_diff,
        home_wins = Wins[Venue == "Home"],
        home_draws = Draws[Venue == "Home"],
        home_losses = Losses[Venue == "Home"],
        away_wins = Wins[Venue == "Away"],
        away_draws = Draws[Venue == "Away"],
        away_losses = Losses[Venue == "Away"]
      )
    
    stadium_analysis <- stadiums %>%
      inner_join(stadium_effect, by = "Team") %>%
      filter(Team %in% top_10_teams)
    
    pal <- colorNumeric(
      palette = c("red3", "white", "green4"),
      domain = c(min(stadium_analysis$stadium_effect), 
                 max(stadium_analysis$stadium_effect))
    )
    
    leaflet(stadium_analysis) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~abs(stadium_effect) * 3,
        color = ~pal(stadium_effect),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<strong>", Team, "</strong><br>",
          "<br>",
          "<strong>Home Record:</strong> ", home_wins, "W-", home_draws, "D-", home_losses, "L<br>",
          "<strong>Away Record:</strong> ", away_wins, "W-", away_draws, "D-", away_losses, "L<br>",
          "<br>",
          "Home xG Difference: ", round(home_xG_diff, 2), "<br>",
          "Away xG Difference: ", round(away_xG_diff, 2), "<br>",
          "Stadium Effect: ", round(stadium_effect, 2)
        )
      ) %>%
      addLabelOnlyMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~paste0(Team, " ", round(stadium_effect, 2)),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "auto",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "12px"
          )
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~stadium_effect,
        title = "Stadium Effect (Home - Away xG Difference)",
        opacity = 0.8
      ) %>%
      setView(
        lng = -2,
        lat = 53,
        zoom = 6
      ) %>%
      addControl(
        html = "<h3>Stadium Effect on Team Performance<br>2023/24 Premier League Season - Top 6 Teams</h3>",
        position = "topright"
      )
  })
  
  # For Stadium Map (in the server section)
  output$map_explanation <- renderUI({
    HTML('
        <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
            <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                Stadium Effect Map
            </h2>
            
            <p style="color: #34495e; line-height: 1.6;">
                This interactive map visualizes the impact of home advantage for each Premier League team during the 2023/24 season.
            </p>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">Map Features:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li><span style="font-weight: bold; color: #2980b9;">Circle Size:</span> Represents the magnitude of stadium effect</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Color Coding:</span> Indicates positive (blue) or negative (red) home advantage</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Interactive Markers:</span> Click for detailed team statistics</li>
                </ul>
            </div>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">How to Use:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li>Click on stadium markers to view team-specific data</li>
                    <li>Compare home performance across different regions</li>
                    <li>Analyze geographical patterns in home advantage</li>
                </ul>
            </div>
        </div>
    ')
  })

output$explanation <- renderUI({
  if(input$plot_type == "scatter") {
    HTML('
            <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                    Goals vs Expected Goals (xG)
                </h2>
                
                <p style="color: #34495e; line-height: 1.6;">
                    This scatter plot compares actual goals scored against expected goals (xG) for each team.
                </p>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Key Elements:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li><span style="font-weight: bold; color: #2980b9;">X-axis:</span> Expected Goals (xG) - the number of goals a team was expected to score based on chance quality</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Y-axis:</span> Actual Goals Scored</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Diagonal Line:</span> Represents where actual goals equal expected goals</li>
                    </ul>
                </div>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Insights:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li><span style="color: #27ae60;">▲</span> Teams above the line outperformed, converting more chances than expected.</li>
                        <li><span style="color: #c0392b;">▼</span> Teams below the line underperformed, scoring fewer goals than predicted.</li>
                    </ul>
                </div>
            </div>
        ')
  } else if(input$plot_type == "passes") {
    HTML('
            <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                    Progressive Passes vs. Assists
                </h2>
                
                <p style="color: #34495e; line-height: 1.6;">
                    This scatter plot visualizes the top players from the top three and bottom three teams based on assists and progressive passes (PrgP).
                </p>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Key Elements:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li><span style="font-weight: bold; color: #2980b9;">Teams:</span> The top and bottom three teams are determined by total assists (Ast)</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Players:</span> The top three players per team are selected based on assists</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Dots:</span> Each dot represents a player, with size indicating total minutes played</li>
                    </ul>
                </div>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Insights:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li>Compare Progressive Passes (PrgP) which are number of passes made by the player that moved the ball forward, and playmaking ability (Ast).</li>
                        <li>Players from top teams often have a higher number of progressive passes and assists, revealing how these teams rely more on individual playmakers.</li>
                    </ul>
                </div>
            </div>
        ')
  } else if(input$plot_type == "treemap") {
    HTML('
            <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                    Player Impact in Top 4 Teams
                </h2>
                
                <p style="color: #34495e; line-height: 1.6;">
                    This treemap plot showcases the impact of players from the top four teams based on their goals, assists, and other key metrics.
                </p>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Metrics:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li><span style="font-weight: bold; color: #2980b9;">Goals per 90:</span> Goals scored per 90 minutes of play</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Assists per 90:</span> Assists provided per 90 minutes</li>
                        <li><span style="font-weight: bold; color: #2980b9;">Total Impact:</span> Combined measure of goals, assists, progressive passes, and carries</li>
                    </ul>
                </div>
                
                <div style="background-color: white; padding: 15px; border-radius: 6px;">
                    <h3 style="color: #2c3e50; margin-bottom: 15px;">Visualization Elements:</h3>
                    <ul style="color: #34495e; line-height: 1.8;">
                        <li>Rectangle size represents overall player impact</li>
                        <li>Color intensity shows the selected metric performance</li>
                        <li>Grouped by team for easy comparison</li>
                    </ul>
                </div>
            </div>
        ')
  } # For Possession Trends
  else if(input$plot_type == "possession") {
    HTML('
        <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
            <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                Possession Trends Analysis
            </h2>
            
            <p style="color: #34495e; line-height: 1.6;">
                This visualization tracks possession patterns over time using a 3-game and a 5-game moving average to smooth out game-to-game variations.
            </p>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">Key Features:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li><span style="font-weight: bold; color: #2980b9;">Moving Average:</span> 3-game and 5-game rolling average showing possession trends</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Team Colors:</span> Distinct colors for each team\'s trend line</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Time Scale:</span> Full season progression by matchweek</li>
                </ul>
            </div>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">How to Interpret:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li>Higher values indicate greater ball control and dominance</li>
                    <li>Observe trend changes following tactical adjustments</li>
                    <li>Compare possession strategies across teams</li>
                </ul>
            </div>
        </div>
    ')
  }
  
  # For Formation Impact
  else if(input$plot_type == "formation") {
    HTML('
        <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
            <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                Formation Impact Analysis
            </h2>
            
            <p style="color: #34495e; line-height: 1.6;">
                This visualization examines how different formations affect team possession and goal efficiency throughout the season.
            </p>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">Metrics Explained:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li><span style="font-weight: bold; color: #2980b9;">Ball Possession (Blue):</span> Percentage of time controlling the ball</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Goal Efficiency (Red):</span> Conversion rate of shots on target to goals</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Formation Markers:</span> Different shapes for each formation used</li>
                </ul>
            </div>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">Key Insights:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li>Compare formation effectiveness across different teams</li>
                    <li>Identify optimal formations for possession vs. efficiency</li>
                    <li>Track tactical evolution throughout the season</li>
                </ul>
            </div>
        </div>
    ')
  }
  
  else if (input$plot_type == "CI")
  {
    HTML('
        <div style="background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
            <h2 style="color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;">
                Goal Efficiency Analysis
            </h2>
            
            <p style="color: #34495e; line-height: 1.6;">
                This forest plot visualizes the goal efficiency (goals per shot on target) for each Premier League team in the 2023/24 season, including confidence intervals for statistical reliability.
            </p>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">Plot Elements:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li><span style="font-weight: bold; color: #2980b9;">Orange Dots:</span> Mean goal efficiency for each team</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Navy Bars:</span> 95% confidence intervals showing uncertainty range</li>
                    <li><span style="font-weight: bold; color: #2980b9;">Dashed Line:</span> League average goal efficiency</li>
                </ul>
            </div>
            
            <div style="background-color: white; padding: 15px; border-radius: 6px;">
                <h3 style="color: #2c3e50; margin-bottom: 15px;">How to Interpret:</h3>
                <ul style="color: #34495e; line-height: 1.8;">
                    <li>Higher values indicate better conversion of shots on target to goals</li>
                    <li>Wider confidence intervals suggest more variable performance</li>
                    <li>Compare team performance to league average (dashed line)</li>
                </ul>
            </div>
        </div>
    ')
    
    
    
  }
  
})
  
}
  
  
# Run the app
shinyApp(ui = ui, server = server)
  