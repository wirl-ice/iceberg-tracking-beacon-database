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
#   - Function to convert beacon data collected by dGPS units (Trimble, TopCon)
#   after conversion to RINEX and .15O files go through PPP (NRCan) before 
#   further processing to 'quality added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - PPP GPS data in csv format
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#
#   PPP-processed GNSS receiver columns (case sensitive):
#   -----------------------------------------------------
#   - beacon
#   - year
#   - month
#   - day
#   - hour
#   - minute
#   - second
#   - lat
#   - lon
#   - ellipsoidal_height_m
#   - ortho_height_m
#   - rcvr_clk_ns
#   - GDOP
#   - obs
#   - gps_time
#
#------------------------------------------------------------------------------

gnss_to_csv <- function(raw_data) {
  
  # Manufacturer sensor range values
  #---------------------------------
  
  # Debug
  message("Executing script: gnss_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "GNSS"
  
  # 3) Data timestamp (UTC)
  if("gps_time" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$gps_time, truncated = 2)
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
  if("lon" %in% names(raw_data)) {
    longitude <- raw_data$lon
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