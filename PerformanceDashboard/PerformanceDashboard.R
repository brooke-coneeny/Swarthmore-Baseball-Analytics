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
current_data <- read_csv("swarthmore_baseball - JeterGame.csv")

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
                        #selectInput("batter", "Batter", c("Austin Burgess", "Jett Shue", "Joe Radek", "Evan Johnson", "Kirk Terada-Herzer", "Benjamin Buchman", "Kaiden Rosenbaum", "Nate Jbara", "Max Roffwarg", "Aidan Sullivan", "Matthew Silvestre", "Xavier Taylor", "Emmet Reynolds", "Max Beadling", "Sam Marco")),
                        selectInput("batter", "Batter", c("Brett Gardner", "Derek Jeter", "Brian McCann", "Mark Teixeira", "Chase Headley", "Chris Young", "Stephen Drew")),
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
  #roster_batter <- c("Austin Burgess", "Jett Shue", "Joe Radek", "Evan Johnson", "Kirk Terada-Herzer", "Benjamin Buchman", "Kaiden Rosenbaum", "Nate Jbara", "Max Roffwarg", "Aidan Sullivan", "Matthew Silvestre", "Xavier Taylor", "Emmet Reynolds", "Max Beadling", "Sam Marco")
  roster_pitcher <- c("Alex Rimerman", "Casey Jordan", "Ethan Weiss", "Jeremy Jensen", "Josh Rankey", "Liam Alpern", "Matt Silvestre", "Steven Jungers")
  roster_batter <- c("Brett Gardner", "Derek Jeter", "Brian McCann", "Mark Teixeira", "Chase Headley", "Chris Young", "Stephen Drew")
  
  # Empty data frames for individual pitching and hitting data 
  pitcher_data <- data.table(matrix(ncol = 16))
  hitting_data <- data.table(matrix(ncol = 16))
  team_hitting <- data.frame(matrix(NA, nrow = length(roster_batter), ncol = 16))
  team_pitching <- data.frame(matrix(NA, nrow = length(roster_pitcher), ncol = 12))
  
  # Creating hitting statistics for the team
  team_hitting_statistics <- data.frame(matrix(NA, ncol = 16, nrow = length(roster_batter)))
  colnames(team_hitting_statistics) <- c("Name", "1B","2B","3B","HR","BB", "H", "AB", "AVG", "OBP", "K%", "BB%", "R", "RBI", "SLG", "OPS")
  for (i in 1:length(roster_batter)) {
    
    # Select pitcher of interest
    current_batter <- roster_batter[i]
    
    # Add to table
    team_hitting_statistics[i,"Name"] <- current_batter
    # Singles
    team_hitting_statistics[i,"1B"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Single") %>% nrow()
    # Doubles
    team_hitting_statistics[i,"2B"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Double") %>% nrow()
    # Triples
    team_hitting_statistics[i,"3B"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Triple") %>% nrow()
    # Home runs
    team_hitting_statistics[i,"HR"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "HR") %>% nrow()
    # Walks
    team_hitting_statistics[i,"BB"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Walk") %>% nrow()
    # Number of at-bats
    team_hitting_statistics[i,"AB"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Out", "Strike Out", "Single", "Double", "Triple", "HR", "Error", "Fielder's Choice", "Double Play", "Triple Play")) %>% nrow()
    # Number of hits
    team_hitting_statistics[i,"H"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Single", "Double", "Triple", "HR")) %>% nrow()
    # Batting Average
    number_at_bats <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Out", "Strike Out", "Single", "Double", "Triple", "HR", "Error", "Fielder's Choice", "Double Play", "Triple Play")) %>% nrow()
    number_hits <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Single", "Double", "Triple", "HR")) %>% nrow()
    team_hitting_statistics[i,"AVG"] <- round((number_hits / number_at_bats),3)
    # On-Base Percentage: (H+BB+HBP) / (AB + BB + HBP + SF)
    number_hbp <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "HBP") %>% nrow()
    number_sf <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Sac Fly") %>% nrow()
    number_plate_appearances <- team_hitting_statistics[i,"AB"] + team_hitting_statistics[i,"BB"] + number_hbp + number_sf
    number_on_base <- team_hitting_statistics[i,"H"] + team_hitting_statistics[i,"BB"] + number_hbp
    team_hitting_statistics[i,"OBP"] <- round((number_on_base / number_plate_appearances),3)
    # Strikeout percentage
    number_strikeouts <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Strike Out")) %>% nrow()
    team_hitting_statistics[i,"K%"] <- round((number_strikeouts / number_plate_appearances),3)
    # Walk percentage
    number_walks <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome %in% c("Walk")) %>% nrow()
    team_hitting_statistics[i,"BB%"] <- round((number_walks / number_plate_appearances),3)
    # Number of Runs
    team_hitting_statistics[i,"R"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Runs > 0) %>% nrow()
    # Number of RBI's
    team_hitting_statistics[i,"RBI"] <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(`RBI's` > 0) %>% nrow()
    # Slugging Percentages: total bases / AB
    total_1B <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Single") %>% nrow()
    total_2B <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Double") %>% nrow()
    total_3B <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "Triple") %>% nrow()
    total_HR <- current_data %>% filter(`Batter's Name` == current_batter) %>% filter(Outcome == "HR") %>% nrow()
    total_bases <- total_1B + total_2B + total_3B + total_HR
    team_hitting_statistics[i,"SLG"] <- round((total_bases / number_at_bats),3)
    # On Base Plus Slugging
    team_hitting_statistics[i,"OPS"] <- team_hitting_statistics[i,"OBP"] + team_hitting_statistics[i,"SLG"] 
  }
  
  # Creating hitting visuals for the team
  
  # Creating pitching statistics for the team
  team_pitching_statistics <- data.frame(matrix(NA, ncol = 12, nrow = length(roster_pitcher)))
  colnames(team_pitching_statistics) <- c("Name", "ERA", "IP", "H", "R", "ER", "BB", "SO", "2B", "3B", "HR", "AB")
  for (i in 1:length(roster_pitcher)) {
    
    # Select pitcher of interest
    current_pitcher <- roster_pitcher[i]
    
    # Add to table
    team_pitching_statistics[i,"Name"] <- current_pitcher
    # Innings Pitcher
    #team_pitching_statistics[i,"IP"] <- current_data %>% filter(`Pitcher's Name` == current_pitcher)
  }
  
  # Making our total data set a reactive value so that it continues to update 
  values <- reactiveValues(Pitchers = pitcher_data, Hitters = hitting_data, TeamHitting = team_hitting)
  
  # Observe pitching event
  current_event <- observeEvent(input$pitcher, {
    values$Pitchers <- current_data %>% filter(`Pitcher's Name` == input$pitcher)
  })
  
  # Observe hitting event
  current_event <- observeEvent(input$batter, {
    values$Hitters <- team_hitting_statistics %>% filter(Name == input$batter)
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

