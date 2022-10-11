##############################################
#        Ideas for scrimmage Data            #
##############################################

### Load in libraries
library(tidyverse)
library(openxlsx)
library(readxl)

### Load in data set
setwd("C:/Users/bcone/Downloads/Swat Baseball/Data")
GameData <- read_excel(path = "TotalInnerSquadFall.xlsx", col_names = TRUE) 

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
    new_data <- as.data.frame(matrix(ncol = ncol(instances)))
    #names(new_data)=c("`Batter's Name`", "`Pitcher's Name`", "Team", "Outting", "Outs","Runners","Strikes", "`Strike Type`", "Balls","Outcome", "`RBI's`", "Runs", "`Away's Runs`", "`Home's Runs`", "instance", "num_instance", "instances_runs")
    colnames(new_data) <- colnames(instances)
    
    # Function: 
    ## Passes in instances_runs: filtered data set to a specific instance 
    ## Passes in inning: the specific inning we are looking at 
    ## Returns data frame with the difference between final score and current score for the inning 
    calc_difference <- function(data) {

      # For each time this situation occurred 
      for (i in 1:nrow(data)) {
        # What Outting is it? 
        curr_outting <- data[i,]$Outting
        
        # Find the score at the end of the current inning 
        score <- instances %>%
          filter(Outting == curr_outting)
        
        end_score <- as.numeric(max(score$`Home's Runs`))

        # Find the score at the time the instance occurred 
        runs <- as.numeric(data$`Home's Runs`[i])

        # Find the difference between final score of inning and current score 
        difference <- end_score - runs
        data$instances_runs[i] = difference 
      }
      return(data)
    }
    
    # For each instance 
    innerSquadInstances <- unique(na.omit(instances$instance, i))
    
    for (current_instance in innerSquadInstances) {
      print(current_instance)
      instances_runs_difference <- instances %>%
        # Filter for a given instance
        filter(instance == as.character(current_instance)) %>%
        mutate(instances_runs = NA)

      # Call function to return data frame with differences calculated 
      # Pass in the data set for that instance
      differences <- calc_difference(instances_runs_difference)

      # Add to data frame
      new_data <- bind_rows(new_data,differences)
    }
  
  # Step 3: Divide by the total number of instances to get the averages 
    RE24_data <- new_data %>%
      # Runs scored / number of instances 
      group_by(instance) %>%
      mutate(total_runs = sum(instances_runs)) %>%
      mutate(RE = total_runs / num_instance)
    
    RE24_viz <- na.omit(RE24_data) %>% ggplot(aes(x = Runners, y = Outs)) +
      geom_tile(aes(fill = RE)) +
      geom_text(aes(label=round(RE,2))) +
      scale_fill_distiller(palette = "Reds", direction = 1) +
      #scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
      labs(
        x = "Runners",
        y = "Outs",
        title = "Run-Expectancy Matrix"
      ) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=14,face="bold"),
            plot.title = element_text(size = 24, hjust = 0.5),
            legend.position = "none")


