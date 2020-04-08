#------------------------------------------------------------------------------
# Title: Deleting replicated files
#
# Created by: Derek Mueller, Carleton University
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, March 7, 2014
#   Adam Garbo, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert standardized beacon csv to speedplot 
#   - Called on by beacon_processing.R
#
# Required file(s): 
#   - Processed/standardized beacon csv file
#
# Notes:
#   - There is an added line to remove rows with NAs when calculating azimuth. 
#   - Is there another way to fix this?
#------------------------------------------------------------------------------

speed_plot <- function(standard_data) {
  
  # Debug
  message("Executing script: speed_plot.R")
  
  # Fidig independent number of days
  doy = as.numeric(strftime(standard_data$datetime_data, "%j"))
  differentDate = doy[1:length(doy) - 1] - doy[2:length(doy)]
  index = which(differentDate != 0) + 1  
  
  # Copy data and subset on index (not a 24 hr sampling but the day changed)
  dayDateTime = standard_data$datetime_data[index]
  dayLat = standard_data$latitude[index]
  dayLon = standard_data$longitude[index]
  daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                              dayDateTime[1:length(dayDateTime) - 1], units = "hours"))
  
  # get the distance travelled between positions 
  dist = vector(mode = 'numeric', length = 0)
  az = vector(mode = 'numeric', length = 0)
  err = vector(mode = 'numeric', length = 0)
  
  for (i in 1:(length(dayLat) - 1) ) {
    vect = distaz(dayLat[i], dayLon[i], dayLat[i + 1], dayLon[i + 1])      
    dist = append(dist, vect$dist)
    az = append(az, vect$az)
    err = append(err, vect$err)
  }    # End distance function
  
  DayDist = data.frame(dist, az, err, daydt, dayDateTime[1:i])
  
  # Get rid of rows with NAs
  DayDist = DayDist[complete.cases(DayDist),]
  
  #calc the speed
  daySpeed = DayDist$dist / DayDist$daydt # in kph
  daySpeed = daySpeed*24 #in km per day
  dayaz = DayDist$az
  
  # get rid of the negatives
  for (i in 1:length(dayaz)) {
    if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
  }    # End fuction to remove negaties
  dayaz = dayaz*pi/180
  
  # Plot speed over time
  plot(DayDist$dayDateTime.1.i., daySpeed, 
       ylab = "Speed (km/d)", 
       xlab = "Date", 
       main = "Ice island daily speed", 
       sub = filename)
  
  #ggplot(DayDist, aes(dayDateTime.1.i., daySpeed, group = 1)) +
  #  geom_point() +
  #  theme_bw()
  
  #Delete existing files
  delete_file(paste("speedPlot_", filename, sep = ""), c("png"))    
  
  #Save plot to output directory
  dev.copy(png, paste("speedplot_", filename, ".png", sep = ""))
  dev.off()
  
} # End fuction