#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Adam Garbo
#
# Date: May 19, 2021
#
# Modified by: 
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw CSV data from IIP ice tracking beacons to
#   standardized CSV before further processing to 'quality added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV)
#
# Notes: 
#   - Beacon data should be separated into separate CSV files.
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - INDEX
#   - ID
#   - DATETIME
#   - LATITUDE
#   - LONGITUDE

#------------------------------------------------------------------------------

iip_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: iip_to_csv.R")
  
  # Manufacturer sensor range values
  #---------------------------------
  vbat_max = 8.0
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "IIP"
  
  # 3) Data timestamp (UTC)
  if("DATETIME" %in% names(raw_data)) {
    # Remove "UTC" from timestamp as this is not compatible ISO standards
    raw_data$DATETIME <- gsub('.{3}$', '', raw_data$DATETIME)
    datetime_data <- anytime(raw_data$DATETIME)
  } else {
    datetime_data <- NA
  }
  
  # 5) Latitude
  if("LATITUDE" %in% names(raw_data)) {
    latitude <- raw_data$LATITUDE
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("LONGITUDE" %in% names(raw_data)) {
    longitude <- raw_data$LONGITUDE
  } else {
    longitude <- NA
  }

  # Put the declared variables into one data frame
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