#load libraries
#library(plyr)
#library(dplyr)
#library(tidyr)
#library(ggplot2)
#library(reshape2)
#library(forcats)
library(readxl)
#library(data.table)
library(magick)
library(openxlsx)
#library(cowplot)
#library(extrafont)
#library(gridExtra)
#library(rlang)
library(reader)
library(rmarkdown)
#library(plotrix)
library(chron)
library(pdftools)
library(tidyverse)

# Location of static pages within folder
setwd("C:/Users/bcone/Downloads/Swat Baseball/Static Pages")

# Static PDF page for report
RPMdoc <- image_read_pdf("SwatBaseballReportBlank.pdf")

# Location of data for practice
setwd("C:/Users/bcone/Downloads/Swat Baseball")

# Collected Rapsodo pitching data 
internal <- read_excel(path = "Swat Baseball Roster 2022_2023.xlsx", col_names = TRUE) 
RapPitch <- read_excel(path = ".xlsx", col_names = TRUE) 

# Format player names to match Rapsodo data files
internal$RapNames <- paste(internal$`First Name`, internal$`Last Name`, sep="")

#specific player 
indPlayerData <- subset(internal[i,])


for (i in 1:nrow(internal)) {
  indPlayerData <- subset(internal[i,])
  
  
  #create pitching sheet 
  if (indPlayerData$RapNames[1] %in% gsub(" ","",RapPitch$`Player Name`)) {
    rapPitchPlayer <- RapPitch %>% filter(gsub(" ","",RapPitch$`Player Name`) == indPlayerData$RapNames[1])
    rapPitchPlayer <- rapPitchPlayer %>% filter(Spin != '-' & rapPitchPlayer$`Spin Efficiency (release)` != '-')
    rapPitchPlayer$Spin <- as.numeric(as.character(rapPitchPlayer$Spin))
    rapPitchPlayer$`Spin Efficiency (release)` <- as.numeric(as.character(rapPitchPlayer$`Spin Efficiency (release)`))
    
    #the spin axes are being uploaded as the proportion that time is out of 24 hours for some entries so made a fix 
    if (indPlayerData$RapNames[1] %in% c("LeviSterling", "CohenGomez", "CooperWilliams", "GavinStedman", "MasonRussell", 
                                         "TannerWaldrop", "BryceNavarre", "Coleman(Kash)Mayfield", "BrendonBennett", 
                                         "RyanSloan", "ZionTheophilus", "JacksonSanders")) {
      
      # For all of the entries 
      for (s in 1:length(rapPitchPlayer$`Spin Axis`)) {
        # If it is of type decimal when converted to numeric format
        if (!is.na(as.numeric(rapPitchPlayer$`Spin Axis`[s]))) {
          # Find the number of hours 
          rapPitchPlayer$`Spin Axis`[s] <- as.numeric(rapPitchPlayer$`Spin Axis`[s]) * 24
          # Convert number of hours to numeric class 
          numeric_spin <- as.numeric(rapPitchPlayer$`Spin Axis`[s])
          # Starting the clock at midnight 
          midnight <- strptime("00:00", format = "%H:%M", tz = "UTC")
          # Calculate the new time 
          time <- midnight + (numeric_spin * 60 * 60)
          # Add this time to the data set in the form of a character 
          rapPitchPlayer$`Spin Axis`[s] <- as.character(time) 
        } else {
          # Else, it turns to NA when converted to numeric type so instead we go to POSIXct to character 
          rapPitchPlayer$`Spin Axis`[s] <- as.character(as.POSIXct(paste(rapPitchPlayer$`Spin Axis`[s]), format = "%H:%M", tz = "UTC"))
        }
      }
      
      # If they are not one of the pitchers who had the weird data, do the calculation 
    } else {
      # Find how many hours past midnight
      rapPitchPlayer$`Spin Axis` <- as.numeric(rapPitchPlayer$`Spin Axis`) * 24
      # Starting at midnight 
      midnight <- strptime("00:00", format = "%H:%M", tz = "UTC")
      # Calculate the new time 
      times <- c(midnight + (rapPitchPlayer$`Spin Axis` * 60 * 60))
      rapPitchPlayer$`Spin Axis` <- times
    }
    
    rapPitchPlayer$`VB (spin)` <- as.numeric(as.character(rapPitchPlayer$`VB (spin)`))
    rapPitchPlayer$`HB (spin)` <- as.numeric(as.character(rapPitchPlayer$`HB (spin)`))
    rapPitchPlayer$`Release Angle` <- as.numeric(as.character(rapPitchPlayer$`Release Angle`))
    rapPitchPlayer$`Release Height` <- as.numeric(as.character(rapPitchPlayer$`Release Height`))
    rapPitchPlayer$`Release Side` <- as.numeric(as.character(rapPitchPlayer$`Release Side`))
    rapPitchPlayer$`Gyro Degree (deg)` <- as.numeric(as.character(rapPitchPlayer$`Gyro Degree (deg)`))
    rapPitchPlayer$`Strike Zone Side` <- as.numeric(as.character(rapPitchPlayer$`Strike Zone Side`))
    rapPitchPlayer$`Strike Zone Height` <- as.numeric(as.character(rapPitchPlayer$`Strike Zone Height`))
    
    #create HB/VB and release location plots
    cols <- c("Fastball" = "blue", "TwoSeamFastball" = "cyan", "CurveBall" = "green3", "ChangeUp" = "darkorange", "Cutter" = "red3", "Slider" = "purple", "Splitter" = "deeppink", "Sinker" = "darkgoldenrod1")
    
    HBVBPlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`HB (spin)`, y = rapPitchPlayer$`VB (spin)`)) + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = TRUE) + 
      scale_color_manual(values = cols) + xlab("Horizontal Break (in)") + ylab("Vertical Break (in)") +  
      theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "bottom") + 
      xlim(-max(abs(rapPitchPlayer$`HB (spin)`)), max(abs(rapPitchPlayer$`HB (spin)`))) + ylim(-max(abs(rapPitchPlayer$`VB (spin)`)), max(abs(rapPitchPlayer$`VB (spin)`))) +
      theme(legend.title= element_blank(), legend.text=element_text(size=6), plot.margin = margin(t = 10, r = 225, b = 10, l = 225)) + guides(colour = guide_legend(nrow = 1))
    
    releasePlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`Release Side`, y = rapPitchPlayer$`Release Height`)) + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = FALSE) + scale_color_manual(values = cols) + 
      xlab("Release Side (ft)") + ylab("Release Height (ft)") +  theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + xlim(-max(abs(rapPitchPlayer$`Release Side`)), max(abs(rapPitchPlayer$`Release Side`))) + 
      ylim(4.5,max(rapPitchPlayer$`Release Height`)) 
    
    if (mean(rapPitchPlayer$`Release Side`) < 0) {
      releasePlot <- ggdraw() + draw_image(image_flop(pitchSilhouette), scale = 0.47, x = 0.05, y = -0.05) + draw_plot(releasePlot)
    }
    if (mean(rapPitchPlayer$`Release Side`) > 0) {
      releasePlot <- ggdraw() + draw_image(pitchSilhouette, scale = 0.47, x = 0.1, y = -0.05) + draw_plot(releasePlot)
    }
    SZPlot <- ggplot(rapPitchPlayer, aes(x = rapPitchPlayer$`Strike Zone Side`, y = rapPitchPlayer$`Strike Zone Height`)) + geom_rect(xmin = -8.5, xmax = 8.5, ymin = 18, ymax = 42, color = "black", fill = "#EEEEEE") + geom_point(aes(color = rapPitchPlayer$`Pitch Type`), show.legend = FALSE) + scale_color_manual(values = cols) + xlab("Strike Zone Side (in)") + ylab("Strike Zone Height (in)") + theme(text = element_text(family = "DINCondensed-Bold", size = 15), panel.grid.major = element_line(linetype = "solid", colour = "#EEEEEE"), legend.position = "none") + xlim(-35,35) + ylim(0,60)
    
    ggsave(filename = paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"HBVB",".png"), plot = HBVBPlot, width=8.875,height=3.7,units="in",dpi=265)
    ggsave(filename = paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"releaseLoc",".png"), plot = releasePlot, width=2.7,height=3,units="in",dpi=265)
    ggsave(filename = paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"strikezone",".png"), plot = SZPlot, width=2.7,height=3,units="in",dpi=265)
    
    rapPitchAvgs <- data.frame(matrix(NA, nrow = length(unique(rapPitchPlayer$`Pitch Type`)), ncol = 16))
    colnames(rapPitchAvgs) <- c("Number of Pitches", "AVG Spin", "MAX Spin", "AVG HB", "MAX HB", "AVG VB", "MAX VB", "Spin Eff", "Gyro Deg", "Spin Dir", "AVG Velo", "MAX Velo", "MIN Velo", "Release Height", "Release Side", "Release Angle")
    row.names(rapPitchAvgs) <- unique(rapPitchPlayer$`Pitch Type`)
    
    absmax <- function(x) { x[which.max( abs(x) )]}
    
    # Reformat spin axis 
    rapPitchPlayer$`Spin Axis` <-  c(strftime(rapPitchPlayer$`Spin Axis`, "%H:%M:%S", tz = "UTC"))
    
    #calculate pitch statistics for each pitch type for player
    for (j in 1:nrow(rapPitchAvgs)) {
      pitch_type <- row.names(rapPitchAvgs)[j]
      rapPitchAvgs[j,"Number of Pitches"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% nrow()
      rapPitchAvgs[j,"AVG Spin"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(Spin)))
      rapPitchAvgs[j,"MAX Spin"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(max(Spin)))
      rapPitchAvgs[j,"AVG HB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`HB (spin)`, na.rm = TRUE))
      rapPitchAvgs[j,"MAX HB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(absmax(`HB (spin)`))
      rapPitchAvgs[j,"AVG VB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`VB (spin)`, na.rm = TRUE))
      rapPitchAvgs[j,"MAX VB"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(absmax(`VB (spin)`))
      rapPitchAvgs[j,"Spin Eff"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`Spin Efficiency (release)`))
      rapPitchAvgs[j,"Gyro Deg"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(mean(`Gyro Degree (deg)`))
      
      rapPitchAvgs[j,"Spin Dir"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(substring(mean(times(`Spin Axis`)), 0, 5))
      
      rapPitchAvgs[j,"AVG Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(Speed), 1))
      rapPitchAvgs[j,"MAX Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(max(Speed), 1))
      rapPitchAvgs[j,"MIN Velo"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(min(Speed), 1))
      rapPitchAvgs[j,"Release Height"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Height`), 1))
      rapPitchAvgs[j,"Release Side"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Side`), 1))
      rapPitchAvgs[j,"Release Angle"] <- rapPitchPlayer %>% filter(`Pitch Type` == pitch_type) %>% summarise(round(mean(`Release Angle`), 1))
    }
    
    RPdoc <- image_read_pdf("Static Pages/2022RapsodoPitchingReport.pdf")
    HBVB_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"HBVB",".png"))
    release_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"releaseLoc",".png"))
    SZ_pic <- image_read(paste0("PDF Full Reports/", internal$`Last Name`[i], internal$`First Name`[i],"strikezone",".png"))
    
    RPdoc <- image_composite(RPdoc, HBVB_pic, offset = "+80+2110")
    RPdoc <- image_composite(RPdoc, release_pic, offset = "+1700+2125")
    RPdoc <- image_composite(RPdoc, SZ_pic, offset = "+100+2125")
    
    if (!is.na(rapPitchAvgs["Fastball", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["Fastball",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-930")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-930")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-930")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-930")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-930")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-930")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Fastball",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-930")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800-130")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Fastball",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020-130")
    }
    
    if (!is.na(rapPitchAvgs["TwoSeamFastball", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["TwoSeamFastball",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-860")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-860")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-860")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-860")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-860")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-860")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["TwoSeamFastball",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-860")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800-50")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["TwoSeamFastball",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020-50")
    }
    
    if (!is.na(rapPitchAvgs["ChangeUp", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["ChangeUp",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-785")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-785")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-785")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-785")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-785")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-785")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["ChangeUp",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-785")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+20")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["ChangeUp",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+20")
    }
    
    if (!is.na(rapPitchAvgs["CurveBall", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["CurveBall",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-710")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-710")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-710")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-710")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-710")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-710")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["CurveBall",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-710")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+95")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["CurveBall",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+95")
    }
    
    if (!is.na(rapPitchAvgs["Slider", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["Slider",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-635")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-635")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-635")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-635")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-635")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-635")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Slider",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-635")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+170")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Slider",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+170")
    }
    
    if (!is.na(rapPitchAvgs["Cutter", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["Cutter",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-560")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-560")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-560")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-560")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-560")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-560")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Cutter",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-560")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+245")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Cutter",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+245")
    }
    
    if (!is.na(rapPitchAvgs["Splitter", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["Splitter",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-480")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-480")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-480")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-480")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-480")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-480")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Splitter",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-480")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+320")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Splitter",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+320")
    }
    
    if (!is.na(rapPitchAvgs["Sinker", 1])) {
      RPdoc <- image_annotate(RPdoc, as.character(rapPitchAvgs["Sinker",1]), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-695-405")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",13], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-410-405")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",11], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-130-405")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",12], 1), " MPH"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150-405")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",8], 1), "%"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+435-405")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",9], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+705-405")
      RPdoc <- image_annotate(RPdoc, rapPitchAvgs["Sinker",10], gravity = "center", font = "DIN Condensed", size = 10.5, location = "+985-405")
      
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",2], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-720+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",3], 0), " RPM"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-500+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",4], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-285+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",5], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "-70+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",6], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+150+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",7], 1), "\""), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+365+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",14], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+580+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",15], 1), "'"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+800+395")
      RPdoc <- image_annotate(RPdoc, paste0(round(rapPitchAvgs["Sinker",16], 1), "°"), gravity = "center", font = "DIN Condensed", size = 10.5, location = "+1020+395")
    } 
    
    
  }
  
  RPdoc <- image_join(RPdoc, RPMdoc)
  
  #write final document to wd
  image_write(RPdoc,path=paste0("Pitching Sheets/", internal$`Last Name`[i],internal$`First Name`[i],"Pitching.pdf"),format="pdf", quality = 100,density = 300)
  
}