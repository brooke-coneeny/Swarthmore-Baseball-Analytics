library(shiny)
library(shinythemes)
library(DT)  
library(tidyverse)
library(data.table)
library(openxlsx)
library(readxl)

library(googlesheets4)
library(googledrive)

library(shinythemes)

# Read in sample data 
innerSquad <- read_excel(path = "TotalInnerSquadFall.xlsx", col_names = TRUE)

# Define UI for application 
ui <- fluidPage(
  
  # Theme
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("Swarthmore Baseball"),
  
  # Navigation
  navbarPage(
    "Performance Dashboard",
    tabPanel("Offense",
             # Filter for specific opponent or all opponents
             navlistPanel(
               tabPanel(title = "Team Statistics",
                        # Output table showing the statistics for the team
                        actionButton("viewTeamHitting", "View Team Stats"),
                        tableOutput("teamHitting"),
               ),
               tabPanel(title = "Visuals"
                        # Graph showing where most of the teams hits fall
                        # Run expectancy Matrix
               ),
               tabPanel(title = "Individuals",
                        # Select the individual you want to examine
                        selectInput("batter", "Batter", c("Austin Burgess", "Jett Shue", "Joe Radek", "Evan Johnson", "Kirk terada-Herze", "Benjamin Buchman", "Kaiden Rosenbaum", "Nate Jbara", "Max Roffwarg", "Aidan Sullivan", "Matthew Silvestre", "Xavier Taylor", "Emmet Reynolds", "Max Beadling", "Sam Marco")),
                        tableOutput("hittingData"),
                            # Output table for their specific statistics
                            # Output visual of where most of their hits fall 
               )
             ) 
    ),
    tabPanel("Defense",
             # Filter for specific opponent or all opponents
             navlistPanel(
               tabPanel(title = "Team Statistics"
                        # Output table showing the statistics for the team
               ),
               tabPanel(title = "Visuals"
                        # Graph showing proportion of outcomes (bar graph)
                        # Matrix showing chance of getting an out given count and runners on base 
                        # Run expectancy matrix of opponents 
               ),
               tabPanel(title = "Individuals",
                        # Select the individual you want to examine
                        selectInput("pitcher", "Pitcher", c("Alex Rimerman", "Casey Jordan", "Ethan Weiss", "Jeremy Jensen", "Josh Rankey", "Liam Alpern", "Matt Silvestre", "Steven Jungers")),
                        tableOutput("pitcherData"),
                        # Graph showing proportion of outcomes (bar graph)
                            # Matrix showing chance of getting an out given count and runners on base 
                            # Run expectancy matrix of opponents 
               )
             )
    ),
  )
)


server <- function(input, output) {
  roster_batter <- c("Austin Burgess", "Jett Shue", "Joe Radek", "Evan Johnson", "Kirk terada-Herze", "Benjamin Buchman", "Kaiden Rosenbaum", "Nate Jbara", "Max Roffwarg", "Aidan Sullivan", "Matthew Silvestre", "Xavier Taylor", "Emmet Reynolds", "Max Beadling", "Sam Marco")
  
  # Empty data frames for individual pitching and hitting data 
  pitcher_data <- data.table(matrix(ncol = 16))
  hitting_data <- data.table(matrix(ncol = 16))
  team_hitting <- data.frame(matrix(NA, nrow = length(roster_batter), ncol = 6))
  
  # Creating hitting statistics for the team
  team_hitting_statistics <- data.frame(matrix(NA, ncol = 6, nrow = length(roster_batter)))
  row.names(team_hitting_statistics) <- unique(roster_batter)
  colnames(team_hitting_statistics) <- c("Name", "1B","2B","3B","HR","BB")
  for (i in 1:length(roster_batter)) {
    
    # Select pitcher of interest
    current_batter <- roster_batter[i]
    
    # Add to table
    team_hitting_statistics[i,"Name"] <- current_batter
    team_hitting_statistics[i,"1B"] <- innerSquad %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Single") %>% nrow()
    team_hitting_statistics[i,"2B"] <- innerSquad %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Double") %>% nrow()
    team_hitting_statistics[i,"3B"] <- innerSquad %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Triple") %>% nrow()
    team_hitting_statistics[i,"HR"] <- innerSquad %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "HR") %>% nrow()
    team_hitting_statistics[i,"BB"] <- innerSquad %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Walk") %>% nrow()
    
  }
  
  # Making our total data set a reactive value so that it continues to update 
  values <- reactiveValues(Pitchers = pitcher_data, Hitters = hitting_data, TeamHitting = team_hitting)
  
  # Observe pitching event
  current_event <- observeEvent(input$pitcher, {
    values$Pitchers <- innerSquad %>% filter(`Pitcher's Name` == input$pitcher)
  })
  
  # Observe hitting event
  current_event <- observeEvent(input$batter, {
    values$Hitters <- innerSquad %>% filter(`Batter's Name` == input$batter)
  })
  
  # Observe team hitting
  current_event <- observeEvent(input$viewTeamHitting, {
    values$TeamHitting <- team_hitting_statistics 
  })
  
  # Show pitcher data 
  output$pitcherData <- renderTable({ values$Pitchers })
  output$hittingData <- renderTable({ values$Hitters })
  output$teamHitting <- renderTable({ values$TeamHitting })
  
}

# Run the application 
shinyApp(
  ui = ui, 
  server = server
)

