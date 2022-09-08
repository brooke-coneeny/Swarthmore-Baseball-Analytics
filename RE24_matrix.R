##############################################
#        Ideas for scrimmage Data            #
##############################################

### Load in libraries
library(tidyverse)

### Load in data set
setwd("C:/Users/bcone/Downloads/Swat Baseball/Trackman")
GameData <- read.csv(path = "GameData.csv", col_names = TRUE) 

### Hard code in the number of runs scored at the end of each inning
scoreboard <- as.data.frame(matrix(ncol = 9, nrow = 2))
# Scores for the away team each inning
scoreboard[1,] = c(2,0,0,0,0,0,0,0,0)
# Scores for the home team each inning 
scoreboard[2,] = c(0,0,0,0,0,0,0,0,0)


# Create a RE24 matrix

  # Step 1: Find all instances of the base-out state from the entire szn (which for us is the scrimmage)
    instances <- GameData %>%
      mutate(instance = case_when(
        # no one on, no outs (NoneZero)
        Runners == "None" & Outs == 0 ~ "NoneZero",
        # no one on, one out (NoneOne)
        Runners == "None" & Outs == 1 ~ "NoneOne",
        # no one on, two outs (NoneTwo)
        Runners == "None" & Outs == 2 ~ "NoneTwo",
        
        # runner on first, no outs (FirstZero)
        Runners == "1st" & Outs == 0 ~ "FirstZero",
        # runner on first, one out (FirstOne)
        Runners == "1st" & Outs == 1 ~ "FirstOne",
        # runner on first, two outs (FirstTwo)
        Runners == "1st" & Outs == 2 ~ "FirstTwo",
        
        # runner on second, no outs
        Runners == "2nd" & Outs == 0 ~ "SecondZero",
        # runner on second, one out
        Runners == "2nd" & Outs == 1 ~ "SecondOne",
        # runner on second, two outs
        Runners == "2nd" & Outs == 2 ~ "SecondTwo",
        
        # runner on third, no outs
        Runners == "3rd" & Outs == 0 ~ "ThirdZero",
        # runner on third, one out
        Runners == "3rd" & Outs == 1 ~ "ThirdOne",
        # runner on third, two outs
        Runners == "3rd" & Outs == 2 ~ "ThirdTwo",
        
        # runner on first and second, no out
        Runners == "1st & 2nd" & Outs == 0 ~ "FirstSecondZero",
        # runner on first and second, one out
        Runners == "1st & 2nd" & Outs == 1 ~ "FirstSecondOne",
        # runner on first and second, two out
        Runners == "1st & 2nd" & Outs == 2 ~ "FirstSecondTwo",
        
        # runner on first and third, no out
        Runners == "1st & 3rd" & Outs == 0 ~ "FirstThirdZero",
        # runner on first and third, one out
        Runners == "1st & 3rd" & Outs == 1 ~ "FirstThirdOne",
        # runner on first and third, two out
        Runners == "1st & 3rd" & Outs == 2 ~ "FirstThirdTwo",
        
        # runner on second and third, no out
        Runners == "2nd & 3rd" & Outs == 0 ~ "SecondThirdZero",
        # runner on second and third, one out
        Runners == "2nd & 3rd" & Outs == 1 ~ "SecondThirdOne",
        # runner on second and third, two out
        Runners == "2nd & 3rd" & Outs == 2 ~ "SecondThirdTwo",
        
        # runner on all bases, no outs
        Runners == "Loaded" & Outs == 0 ~ "LoadedZero",
        # runner on all bases, one out
        Runners == "Loaded" & Outs == 1 ~ "LoadedOne",
        # runner on all bases, two outs
        Runners == "Loaded" & Outs == 2 ~ "LoadedTwo"
      )) %>%
      # Group by instance, then count how many of each 
      group_by(instance) %>%
      mutate(num_instance = n())
      
  # Step 2: Find the total number runs scored from the time each state occurred until the end of the inning
    instance_list <- c("NoneZero", "NoneOne", "NoneTwo", "FirstZero", "FirstOne", "FirstTwo", "SecondZero", "SecondOne", "SecondTwo",
                     "ThirdZero", "ThirdOne", "ThirdTwo", "FirstSecondZero", "FirstSecondOne", "FirstSecondTwo", "FirstThirdZero",
                     "FirstThirdOne", "FirstThirdTwo", "SecondThirdZero", "SecondThirdOne", "SecondThirdTwo", "LoadedZero", "LoadedOne",
                     "LoadedTwo")
    
    # Data frame which will hold the new data 
    new_data <- as.data.frame(matrix(ncol = ncol(instances) + 1))
    names(new_data)=c("Batter.s.Name", "Pitcher.s.Name", "Team", "Inning", "Outs","Runners","Strikes","Balls","Outcome", "RBI.s", "Runs", "Away.s.Runs", "Home.s.Runs", "instance", "num_instance", "instances_runs")
    
    # Function: 
    ## Passes in instances_runs: filtered data set to a specific instance 
    ## Passes in inning: the specific inning we are looking at 
    ## Returns data frame with the difference between final score and current score for the inning 
    calc_difference <- function(data, inning) {
      for (k in 1:nrow(data)) {
        end_score <- scoreboard[1,inning]
        score <- as.numeric(data[k,"Away.s.Runs"])
        difference <- end_score - score
        data$instances_runs[k] = difference 
      }
            return(data)
    }
    
    # For each inning played 
    num_innings <- 1
    for (i in 1:num_innings) {
      # For each instance 
      for (current_instance in instance_list) {
        instances_runs_difference <- instances %>%
          # Away team first 
          filter(Team == as.character(" Away")) %>%
          # Filter for a given instance
          filter(instance == as.character(current_instance)) %>%
          mutate(instances_runs = NA)
        
        # Call function to return data frame with differences calculated 
        # Pass in the data set for that instance and the corresponding inning (i)
        differences <- calc_difference(instances_runs_difference, i)

        # Add to data frame
        new_data <- rbind(new_data,differences)
      }
    }
  
  # Step 3: Divide by the total number of instances to get the averages 
    RE24_data <- new_data %>%
      # Runs scored / number of instances 
      mutate(RE = instances_runs / num_instance)


