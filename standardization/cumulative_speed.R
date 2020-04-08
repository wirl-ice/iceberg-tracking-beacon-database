#------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Derek Mueller, Carleton University
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, March 6, 2014
#   Adam Garbo, March 21, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert standardized beacon CSV to cummulative speed plot 
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Processed/standardized beacon csv file
#------------------------------------------------------------------------------

cumulative_speed <- function(standard_data) {
  
  # Debug
  message("Executing script: cumulative_speed.R")
  
  # Calculating independent number of days
  doy  =  as.numeric(strftime(standard_data$datetime_data, "%j"))
  differentDate = doy[1:length(doy)-1]-doy[2:length(doy)]
  index = which(differentDate != 0)+1  
  
  # Copy data and subset on index (not a 24 hr sampling but the day changed)
  dayDateTime = standard_data$datetime_data[index]
  dayLat = standard_data$latitude[index]
  dayLon = standard_data$longitude[index]
  daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                              dayDateTime[1:length(dayDateTime)-1], units = "hours"))
  
  # Get the distance travelled between positions 
  dist = vector(mode = 'numeric', length = 0)
  az = vector(mode = 'numeric', length = 0)
  err = vector(mode  =  'numeric', length = 0)
  
  for (i in 1:(length(dayLat)-1) ) {
    vect = distaz(dayLat[i], dayLon[i], dayLat[i+1], dayLon[i+1])      
    dist = append(dist, vect$dist)
    az = append(az, vect$az)
    err = append(err, vect$err)
  }    # End distance function
  
  DayDist = data.frame(dist, az, err, daydt, dayDateTime[1:i])
  # Get rid of rows with NAs
  DayDist = DayDist[complete.cases(DayDist),]
  
  # Calculate the speed
  daySpeed = DayDist$dist/DayDist$daydt # in kph
  daySpeed = daySpeed*24 #in km per day
  dayaz = DayDist$az
  
  # Get rid of the negatives
  for (i in 1:length(dayaz)) {
    if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
  }    # End negative function
  
  dayaz = dayaz*pi/180
  
  # Plot cummulative density function for speed vs probability
  plot((ecdf(daySpeed)), ylab = "Probability", xlab = "Speed [km/day]",
       main = "Cumulative density function")
  
  # Delete existing files
  delete_file(paste("cumulative_speed", substring(filename,1,5), sep = ""), c("png"))
  
  # Save to output directory
  setwd(output)
  dev.copy(png, paste("cumulative_speed_", substring(filename,1,5), ".png", sep = ""))
  dev.off()
  
} # End function