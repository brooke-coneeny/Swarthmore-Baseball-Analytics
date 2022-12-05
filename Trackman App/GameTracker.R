library(shiny)
library(shinythemes)
library(DT)  
library(tidyverse)
library(data.table)
library(openxlsx)

library(googlesheets4)
library(googledrive)

# Authenticating Google Drive 
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "Swarthmore-Baseball-Analytics/.secrets"
)

# Get the ID of the sheet for writing programmatically
sheet_id <- drive_get("swarthmore_baseball")$id

# Define UI for application 
ui <- fluidPage(
  
    # Application title
    titlePanel("Swarthmore Baseball"),
    
    # Navigation
    navbarPage(
      "Trackman",
      tabPanel("Game Information",
               # Text Input: Game title
               textInput("date", "Date", " "),
               verbatimTextOutput("title"),
               
               # Text Input: Home team
               textInput("home", "Home", " "),
               verbatimTextOutput("home"),
               
               # Text Input: Away team
               textInput("away", "Away", " "),
               verbatimTextOutput("away"),
               
               # Output scoreboard  
               tableOutput("table3"),
               
      ),
      tabPanel("Play Information",
               sidebarPanel(
                 # Text Input: who is up to bat?
                 textInput("batterName", "Batter's Name", " "),
                 verbatimTextOutput("batterName"),
                 
                 # Text Input: who is pitching? 
                 textInput("pitcherName", "Pitcher's Name", " "),
                 verbatimTextOutput("pitcherName"),
                 
                 # Text Input: home or away? 
                 textInput("team", "Team Hitting", " "),
                 verbatimTextOutput("team"),
                 
                 # Select Input: what inning is it?
                 textInput("inning", "Inning", "1"),
                 verbatimTextOutput("inning"),
                 
                 # Select Input: how many outs are there?
                 selectInput("numberOuts", "Outs", c(0,1,2,3)),
                 
                 # Select Input: where are the runners?
                 selectInput("baseRunners", "Runners on Base", c("None", "1st", "2nd", "3rd", "1st & 2nd", "1st & 3rd", "2nd & 3rd", "Loaded")),
                 
                 # Button: submit data that goes alongside the pitch thrown
                 actionButton("submit", "Submit"),
                 
                 # Button: saves the data 
                 actionButton("end", "Save Data"),
               ),
               mainPanel(
                 # Button: what is the count?        
                 selectInput("strike", "Strike", c(0,1,2,3)),
                 selectInput("strikeType", "Strike Type", c("NA", "Swinging", "Looking", "Foul")),
                 selectInput("ball", "Ball", c(0,1,2,3,4)),
                 
                 # Button: what is the outcome of the at bat? 
                 selectInput("outcome", "Outcome", c("NULL", "Out", "Strike Out", "Single", "Double", "Triple", "HR", "Walk", "HBP", "Sac Fly", "Sac Bunt", "Squeeze Bunt", "Error", "Intentional Walk", "Fielder's Choice", "Double Play", "Triple Play")),
                 
                 # Select Input: how many RBI's
                 selectInput("rbi", "RBI's", c(0,1,2,3,4)),
                 
                 # Select Input: how many runs
                 selectInput("runs", "Runs", c(0,1,2,3,4)),
                 
                 # Text Input: how many runs score that inning? 
                 numericInput("runshome", "Home's Runs", 0),
                 numericInput("runsaway", "Away's Runs", 0),
               ),
        ),
        tabPanel("Table Output",
               # Output tables 
               tableOutput("table1"),
               tableOutput("table2"),
        )
      )
)


server <- function(input, output) {

  # Data table holding the entire play-by-play for the game 
  individualData = data.table(matrix(ncol = 15))
  names(individualData)=c("Date", "Batter's Name", "Pitcher's Name", "Team", "Inning", "Outs","Runners","Strikes", "Strike Type", "Balls","Outcome", "RBI's", "Runs", "Away's Runs", "Home's Runs")
  
  # Data table holding the play-by-play for the current pitch
  temp = as.data.frame(matrix(data = NA, nrow = 1, ncol = 15))
  names(temp)=c("Date", "Batter's Name", "Pitcher's Name", "Team", "Inning", "Outs","Runners","Strikes", "Strike Type", "Balls","Outcome", "RBI's", "Runs", "Away's Runs", "Home's Runs")
  
  # Data table holding the score of the game 
  scoreBoard = as.data.frame(matrix(data = NA, ncol = 10, nrow = 2))
  colnames(scoreBoard) <- c("Team", 1,2,3,4,5,6,7,8,9)
  scoreBoard[1,"Team"] <- "Away"
  scoreBoard[2,"Team"] <- "Home"

  # Making our total data set a reactive value so that it continues to update 
  values <- reactiveValues(Total = individualData, Part = temp, Score = scoreBoard)

  # Current ball in play 
  current_event <- observeEvent(input$submit, {
    
    # Enter the data into the table with the current play 
    values$Part[1,]$`Date`=input$date
    values$Part[1,]$`Batter's Name`=input$batterName
    values$Part[1,]$`Pitcher's Name`=input$pitcherName
    values$Part[1,]$Team=input$team
    values$Part[1,]$Inning=input$inning
    values$Part[1,]$Outs=input$numberOuts
    values$Part[1,]$Runners=input$baseRunners
    values$Part[1,]$Strikes=input$strike
    values$Part[1,]$`Strike Type`=input$strikeType
    values$Part[1,]$Balls=input$ball   
    values$Part[1,]$Outcome=input$outcome
    values$Part[1,]$`RBI's`=input$rbi
    values$Part[1,]$Runs=input$runs
    
    # Enter score into both total data set and score board
    values$Part[1,]$`Away's Runs`=input$runsaway
    values$Part[1,]$`Home's Runs`=input$runshome
    
    # Enter data into score board
    current_inning <- as.numeric(input$inning) + 1
    away_runs <- round(as.numeric(input$runsaway), 0)
    home_runs <- round(as.numeric(input$runshome), 0)

    values$Score[1,current_inning] <- away_runs
    print(away_runs)
    values$Score[2,current_inning] <- home_runs
    
    # Add this to main dataset 
    values$Total <- rbind(values$Total,values$Part)
    print(values$Total)
    print(values$Score)
  })
  
  # Show the user the row of data most recently added to the dataset
  output$table1 <- renderTable({ values$Part[1,] })
  
  # Display the entire data set 
  output$table2 <- renderTable({ values$Total })

  # Display scoreboard
  output$table3 <- renderTable({ values$Score })
  
  # Save data to a csv
  save_event <- observeEvent(input$end, {
    #title <- paste0(input$date, ".csv")
    #print(title)
    #readr::write_csv(values$Total, title)
    
    # Table with all of the data 
    individualData <- values$Total

    # Read in the sheet
    values <- read_sheet(ss = sheet_id, sheet = "main")
    
    # Check to see if our sheet has any data, if not write to it and set up column names,
    # Otherwise, append to it
    if (nrow(values) == 0) {
      sheet_write(data = individualData,
                  ss = sheet_id,
                  sheet = "main")
    } else {
      sheet_append(data = individualData,
                   ss = sheet_id,
                   sheet = "main")
    }
    
    stopApp(returnValue = invisible())
  })
 
}

# Run the application 
shinyApp(
  ui = ui, 
  server = server
)

