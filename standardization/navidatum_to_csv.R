#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Adam Garbo, 
#
# Date: June 17, 2019
#
# Modified by: 
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert a csv of raw data from a Navidatum tracking beacon to a standardized csv before
#   further processing to 'quality added' files
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Navidatum raw beacon data (csv)
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#   
#   Raw data columns (case sensitive):
#   -----------------------------------
#   - Date
#   - Latitude
#   - Longitude
#
#------------------------------------------------------------------------------

navidatum_to_csv <- function(raw_data) {
  
  # Manufacturer sensor range values
  
  # Debug
  message("Executing script: navidatum_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "NAVIDATUM"
  
  # 3) Data timestamp (UTC)
  if("Date" %in% names(raw_data)) {
    datetime_data <- dmy_hms(raw_data$Date, truncated = 2)
  } else {
    datetime_data <- NA
  }
  
  # 5) Latitude
  if("Latitude" %in% names(raw_data)) {
    latitude <- raw_data$Latitude
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("Longitude" %in% names(raw_data)) {
    longitude <- raw_data$Longitude
  } else {
    longitude <- NA
  }
  
  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                              latitude, longitude, row.names=NULL)
  
  # Call data cleaning function
  cleaned_data <- clean_data(processed_data)
  
  # Standardize columns
  standardized_data <- standardize_columns(cleaned_data)
  
  # Calculate distance and speed
  standardized_data <- calculate_speed(standardized_data)
  
  # Choose file name
  output_file = filename
  
  # Choose output directory 
  setwd(output)
  write.csv(standardized_data, paste(output_file, ".csv", sep = ""), row.names=FALSE)
  
} # End function