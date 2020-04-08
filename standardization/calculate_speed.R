#------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Adam Garbo
#
# Date: June 19, 2019
#
# Modified by: 
#   
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Calculates the shortest distance between two points on an ellipsoid 
#   (WGS84 ellipsoid), called the geodesic.
#
# Required file(s): 
#   - Standardized data CSV file
#
# Notes: 
#   
#   
#------------------------------------------------------------------------------

calculate_speed <- function(standardized_data) {
  
  # Debug
  message("Executing script: calculate_speed.R")
  
  # Calcuate distance between all points in metres
  for (i in 1:(length(standardized_data$latitude))) {
    standardized_data$distance[i] = distGeo(c(standardized_data$longitude[i], standardized_data$latitude[i]), 
                                        c(standardized_data$longitude[i+1], standardized_data$latitude[i+1]))
  }
  
  # Round distance to 2 decimal places
  standardized_data$distance <- round(standardized_data$distance, digits = 2)
  
  # Get the time difference between positions
  dt_hrs = as.numeric(difftime(standardized_data$datetime_data[2:length(standardized_data$datetime_data)],
                               standardized_data$datetime_data[1:length(standardized_data$datetime_data)-1], units = "secs"))
  
  # Calculate the speed in metres per second (ms^-1)
  for (i in 1:(length(standardized_data$latitude))) {
    standardized_data$speed[i] = standardized_data$distance[i]/(dt_hrs[i])
  }
  standardized_data$speed <- round(standardized_data$speed, digits = 6)
  
  return(standardized_data)
    
} # End function
