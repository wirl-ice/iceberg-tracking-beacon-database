#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University 
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, Carleton University, April 26, 2018
#   Adam Garbo, Carleton University, June 18, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Descrition: 
#   - Function to convert beacon data collected by Solara iceberg trackers 
#   into a standardized CSV before further processing to 'quality added' files.
#   - Called on by beacon_processing.R  
#
# Necessary file(s): 
#   - Solara raw beacon data in CSV format
#
# Notes:
#   - Raw data should be kept in an 'input' directory with other raw data.
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - serial
#   - alias
#   - lat
#   - long
#   - timestamp 
#------------------------------------------------------------------------------

solara_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: solara_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "SOLARA"
  
  # 3) Data timestamp (UTC)
  if("timestamp" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$timestamp, truncated = 2)
  } else {
    datetime_data <- NA
  }
  
  # 5) Latitude
  if("lat" %in% names(raw_data)) {
    latitude <- raw_data$lat
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("long" %in% names(raw_data)) {
    longitude <- raw_data$long
  } else {
    longitude <- NA
  }

  # Put the declared variables into one data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, latitude, 
                              longitude, row.names=NULL)
  
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