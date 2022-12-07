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
               ),
               tabPanel(title = "Visuals"
                        # Graph showing where most of the teams hits fall
                        # Run expectancy Matrix
               ),
               tabPanel(title = "Individuals",
                        # Select the individual you want to examine
                        selectInput("batter", "Batter", c("Austin Burgess", "Jett Shue", "Joe Radek", "Evan Johnson", "Kirk terada-Herze", "Benjamin Buchman", "Kaiden Rosenbaum", "Nate Jbara", "Max Roffwarg", "Aidan Sullivan", "Matthew Silvestre", "Xavier Taylor", "Emmet Reynolds", "Max Beadling", "Sam Marco")),
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
  # Making our total data set a reactive value so that it continues to update 
  values <- reactiveValues(Pitchers = pitcher_data)
  
  # Observe event
  current_event <- observeEvent(input$pitcher, {
    print(input$pitcher)
    values$pitcher_data <- innerSquad %>% filter(`Pitcher's Name` == input$pitcher)
    print(head(values$pitcher_data))
  })
  
  # Show pitcher data 
  output$pitcherData <- renderTable({ values$pitcher_data })
}

# Run the application 
shinyApp(
  ui = ui, 
  server = server
)

