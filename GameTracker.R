
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
      
      # Select Input: how many outs are there?
      radioButtons("numberOuts", "Outs", c(0,1,2,3)),
        
      # Select Input: where are the runners?
      checkboxGroupInput("baseRunners", "Runners on Base", c("None", "1st", "2nd", "3rd", "1st & 2nd", "1st & 3rd", "2nd & 3rd", "Loaded")),
        
      # Button: what is the count?        
      selectInput("strike", "Strike", c(0,1,2,3)),
      selectInput("ball", "Ball", c(0,1,2,3,4)),
        
      # Button: what is the outcome of the at bat? 
      selectInput("outcome", "Outcome", c("NULL", "Out", "Single", "Double", "Triple", "HR", "Walk", "HBP", "Sac Fly", "Sac Bunt", "Squeeze Bunt", "Error", "Intentional Walk")),
      
      # Button: submit count
      actionButton("submit", "Submit"),
      
      # Button: add count 
      actionButton("append", "Append"),
      
      # Button: end session
      actionButton("end", "End Session")
    ),
    
    mainPanel(
      tableOutput("table")
    )
    
)


server <- function(input, output) {

  # Data table holding the entire play-by-play for the game 
  individualData = data.table(matrix(data = NA, ncol = 6))
  names(individualData)=c("Name","Outs","Runners","Strikes","Balls","Outcome")
  
  # Data table holding the play-by-play for the current pitch
  temp = as.data.frame(matrix(data = NA, nrow = 1, ncol = 6))
  names(temp)=c("Name","Outs","Runners","Strikes","Balls","Outcome")
  
  values <- reactiveValues(Total = individualData, Part = temp)
  
  current_event <- observeEvent(input$submit, {
    
    # Enter the data into the table with the current play 
    values$Part[1,]$Name=input$batterName
    values$Part[1,]$Outs=input$numberOuts
    values$Part[1,]$Runners=input$baseRunners
    values$Part[1,]$Strikes=input$strike
    values$Part[1,]$Balls=input$ball   
    values$Part[1,]$Outcome=input$outcome
    
    # Return the data 
    print(values$Part)
    return(values$Part)
  })
  
  observeEvent(input$append, {
    values$Total <- rbind(values$Total,values$Part)
    print(values$Total)
  })
  
  
  #output$end <- downloadHandler(
    #filename = function() {
     # paste("GameData", ".csv", sep = "")
    #},
    #content = function(file) {
     # write.csv(individualData, file, row.names = FALSE)
    #}
  #)
}

# Run the application 
shinyApp(ui = ui, server = server)
