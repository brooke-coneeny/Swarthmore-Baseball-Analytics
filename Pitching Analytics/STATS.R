#########################################
#        Extra Pitching Stats           #
#########################################

# Load in libraries
library(tidyverse)
library(openxlsx)
library(readxl)

# Load in team inner squad data 
setwd("C:/Users/bcone/Downloads/Swat Baseball/Data")
innerSquad <- read_excel(path = "TotalInnerSquadFall.xlsx", col_names = TRUE) 

# List of pitchers 
pitchers <- c("Alex Rimerman", "Casey Jordan", "Ethan Weiss", "Jeremy Jensen", "Josh Rankey", "Liam Alpern", "Matt Silvestre", "Steven Jungers")

# Output Table
pitchStats <- data.frame(matrix(NA, nrow = length(pitchers), ncol = 3))
colnames(pitchStats) <- c("P/PA", "Swing-Miss%", "K%")
row.names(pitchStats) <- unique(pitchers)

# For each of the pitchers 
for (i in 1:length(pitchers)) {
  
  # Select pitcher of interest
  current_pitcher <- pitchers[i]
  
  # Total pitches thrown in data set 
  total_pitch <- innerSquad %>%
    filter(`Pitcher's Name` == current_pitcher) %>% nrow()
  
  # Avg number pitches per batter 
  avg_pitches <- innerSquad %>%
    filter(`Pitcher's Name` == current_pitcher) %>%
    group_by(`Batter's Name`, Outting) %>%
    mutate(total_pitches = n())
  
  avg_pitches_per_batter <- mean(avg_pitches$total_pitches)
  
  # Ignore the inner squad where we did not have strike type 
  # Swing and miss rate 
  swing_miss <- innerSquad %>%
    filter(`Strike Type` %in% c("NA", "Swinging", "Looking", "Foul")) %>%
    # Filter to only specific player
    filter(`Pitcher's Name` == current_pitcher,
           `Strike Type` == "Swinging") %>% nrow()
  # Number of pitches in dataset (ignoring no strike type data)
  total_pitch_strikeType <- innerSquad %>%
    filter(`Strike Type` %in% c("NA", "Swinging", "Looking", "Foul")) %>%
    filter(`Pitcher's Name` == current_pitcher) %>% nrow()
  
  # Number of batters faced in dataset 
  num_batters <- innerSquad %>%
    filter(`Pitcher's Name` == current_pitcher) %>%
    group_by(`Batter's Name`, Outting) %>%
    n_distinct()
  
  # Strike out percentage 
  num_strikeouts <- innerSquad %>%
    filter(`Pitcher's Name` == current_pitcher,
           Outcome == "Strike Out") %>% nrow()
  
  # Add to table
  pitchStats[i,"P/PA"] <- round(avg_pitches_per_batter,2)
  pitchStats[i,"Swing-Miss%"] <- round(((swing_miss / total_pitch_strikeType)*100),2)
  pitchStats[i,"K%"] <- round(((num_strikeouts / num_batters)*100),2)
  
  
}






