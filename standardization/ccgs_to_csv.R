#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, April 26, 2018
#   Adam Garbo, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw data (CSV) from a self-location datum marking 
#   buoy deployed by the CCGS in 2011 (beacon make/model unknown) to a 
#   standardized CSV before further processing to 'quality added' files. 
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV) from CCGS-owned beacons deployed on PII-A in 2011.
#
# Notes:
#   - Raw data should be kept in an 'input' directory with other raw data.
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - Buoy Name
#   - Time
#   - Latitude
#   - Longitude
#   - Temperature
#   - Drogue Depth (m)
#
#------------------------------------------------------------------------------

ccgs_to_csv  <- function(raw_data) {
  
  # Manufacturer sensor range values
  #---------------------------------
  
  # Debug
  message("Executing script: ccgs_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "CCGS"
  
  # 3) Data timestamp (UTC)
  if("Time" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$Time, truncated = 2)
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
  
  # 9) Internal temperature
  if("Temperature" %in% names(raw_data)) {
    ti <- raw_data$Temperature
  } else {
    ti <- NA
  }
  
  # Put the declared variables into one data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                              latitude, longitude, ti, row.names=NULL)
  
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