library(shiny)
library(shinythemes)
library(DT)  
library(tidyverse)
library(data.table)
library(openxlsx)

library(googlesheets4)
library(googledrive)

library(shinythemes)

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
               tabPanel(title = "Team Statistics"
                        # Output table showing the statistics for the team
               ),
               tabPanel(title = "Visuals"
                        # Graph showing where most of the teams hits fall
                        # Run expectancy Matrix
               ),
               tabPanel(title = "Individuals"
                        # Select the individual you want to examine
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
                            # Graph showing proportion of outcomes (bar graph)
                            # Matrix showing chance of getting an out given count and runners on base 
                            # Run expectancy matrix of opponents 
               )
             )
    ),
  )
)


server <- function(input, output) {
  
}

# Run the application 
shinyApp(
  ui = ui, 
  server = server
)

