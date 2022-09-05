
library(shiny)
library(DT)
library(tidyverse)


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
      checkboxGroupInput("baseRunners", "Runners on Base", c("None", "1st", "2nd", "3rd")),
        
      # Button: what is the count?        
      selectInput("strike", "Strike", c(0,1,2,3)),
      selectInput("ball", "Ball", c(0,1,2,3,4)),
        
      # Button: what is the outcome of the at bat? 
      selectInput("outcome", "Outcome", c("NULL", "Out", "Single", "Double", "Triple", "HR", "Walk", "HBP", "Sac Fly", "Sac Bunt", "Squeeze Bunt", "Error", "Intentional Walk")),
      
      # Button: submit count
      actionButton("submit", "Submit"),
      
      # Button: end session
      actionButton("end", "End Session")
    ),
    
  
    mainPanel(uiOutput('table'))
    
)


server <- function(input, output) {

  individualData = reactiveValues(d1 = as.data.frame(matrix(nrow = 1, ncol = 6)))
  gameData = as.data.frame(matrix(nrow = 3, ncol = 6))
  names(gameData)=c("Name","Outs","Runners","Strikes","Balls","Outcome")
  
  
  observeEvent(input$submit, {
    
    temp = individualData$d1
    names(temp)=c("Name","Outs","Runners","Strikes","Balls","Outcome")
    temp$Name=input$batterName
    temp$Outs=input$numberOuts
    temp$Runners=input$baseRunners
    temp$Strikes=input$strike
    temp$Balls=input$ball
    temp$Outcome=input$outcome
      
    individualData$d1 = temp
    rbind(gameData,individualData$d1)
  })
  
  output$first=renderTable({
    individualData$da
  })
  
  observe({print(individualData$d1)})  # check console output
  observe({print(gameData)})  # check console output
  
}

# Run the application 
shinyApp(ui = ui, server = server)
