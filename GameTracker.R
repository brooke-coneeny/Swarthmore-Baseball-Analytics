
library(shiny)
library(DT) # for data tables 
library(tidyverse)
library(data.table)

library(openxlsx)
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Game Tracker"),
    
    sidebarPanel(
      # Text Input: who is up to bat?
      textInput("batterName", "Batter's Name", " "),
      verbatimTextOutput("batterName"),
      
      # Text Input: who is pitching? 
      textInput("pitcherName", "Pitcher's Name", " "),
      verbatimTextOutput("pitcherName"),
      
      # Select Input: how many outs are there?
      selectInput("numberOuts", "Outs", c(0,1,2,3)),
        
      # Select Input: where are the runners?
      selectInput("baseRunners", "Runners on Base", c("None", "1st", "2nd", "3rd", "1st & 2nd", "1st & 3rd", "2nd & 3rd", "Loaded")),
        
      # Button: what is the count?        
      selectInput("strike", "Strike", c(0,1,2,3)),
      selectInput("ball", "Ball", c(0,1,2,3,4)),
        
      # Button: what is the outcome of the at bat? 
      selectInput("outcome", "Outcome", c("NULL", "Out", "Single", "Double", "Triple", "HR", "Walk", "HBP", "Sac Fly", "Sac Bunt", "Squeeze Bunt", "Error", "Intentional Walk")),
      
      # Select Input: how many RBI's
      selectInput("rbi", "RBI's", c(0,1,2,3,4)),
      
      # Button: submit count
      actionButton("submit", "Submit"),
      
      # Button: end session
      actionButton("end", "End Session")
    ),
    
    mainPanel(
      tableOutput("table1"),
      tableOutput("table2")
    )
    
)


server <- function(input, output) {

  # Data table holding the entire play-by-play for the game 
  individualData = data.table(matrix(data = NA, ncol = 8))
  names(individualData)=c("Batter's Name", "Pitcher's Name","Outs","Runners","Strikes","Balls","Outcome", "RBI's")
  
  # Data table holding the play-by-play for the current pitch
  temp = as.data.frame(matrix(data = NA, nrow = 1, ncol = 8))
  names(temp)=c("Batter's Name", "Pitcher's Name","Outs","Runners","Strikes","Balls","Outcome", "RBI's")
  
  # Making our total data set a reactive value so that it continues to update 
  values <- reactiveValues(Total = individualData, Part = temp)
  
  # Current ball in play 
  current_event <- observeEvent(input$submit, {
    
    # Enter the data into the table with the current play 
    values$Part[1,]$`Batter's Name`=input$batterName
    values$Part[1,]$`Pitcher's Name`=input$pitcherName
    values$Part[1,]$Outs=input$numberOuts
    values$Part[1,]$Runners=input$baseRunners
    values$Part[1,]$Strikes=input$strike
    values$Part[1,]$Balls=input$ball   
    values$Part[1,]$Outcome=input$outcome
    values$Part[1,]$`RBI's`=input$rbi
    
    # Add this to main dataset 
    values$Total <- rbind(values$Total,values$Part)
    print(values$Total)
  })
  
  output$table1 <- renderTable({ values$Part[1,] })
  output$table2 <- renderTable({ values$Total })
  
  # Save data to an excel
  save_event <- observeEvent(input$end, {
    readr::write_csv(values$Total, "GameData.csv")
    print("saved data")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
