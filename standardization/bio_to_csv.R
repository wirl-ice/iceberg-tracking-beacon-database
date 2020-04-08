#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, Carleton University, April 26, 2018
#   Adam Garbo, Carleton University, June 16, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw data from a GPS/Iridium beacon developed at the 
#   Bedford Institute ofOceanography and used by the Department of Fisheries 
#   and Oceans to a standardized csv before  further processing to 'quality 
#   added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data
# 
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#
#   Raw data columns (case sensitive):
#   ---------------------------------
#   - BID
#   - VOLTAGE
#   - GPS_DATE
#   - LATITUDE
#   - LONGITUDE
#------------------------------------------------------------------------------

bio_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: bio_to_csv.R")
  
  # Manufacturer sensor range values
  #---------------------------------

  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "BIO"
  
  # 3) Data timestamp (UTC)
  if("GPS_DATE" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$GPS_DATE, truncated = 2)
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
  
  # 7) Battery voltage
  if("VOLTAGE" %in% names(raw_data)) {
    vbat <- raw_data$VOLTAGE
  } else {
    vbat <- NA
  }
  
  # Put the declared variables into one data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                              latitude, longitude, vbat, row.names=NULL)
  
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