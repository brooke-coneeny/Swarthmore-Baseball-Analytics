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
    tabPanel("Game Information"),
    tabPanel("Play Information"),
    tabPanel("Table Output")
  )
)


server <- function(input, output) {
  
}

# Run the application 
shinyApp(
  ui = ui, 
  server = server
)

