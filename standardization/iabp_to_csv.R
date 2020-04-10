#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, April 26, 2018
#   Adam Garbo, June 17, 2018
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw data downloaded from the International Arctic 
#   Buoy Program website to a standardized csv before further processing to 
#   'quality added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV)
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#   - Note: Beacon type has to be checked and filled in - this just deals with 
#   format of data from IABPrepository (but many different beacon types report 
#   there).
#
#  Raw data columns (case sensitive):
# -----------------------------------
#   - BuoyID
#   - Date
#   - Year 
#   - Hour 
#   - Min 
#   - DOY 
#   - POS_DOY 
#   - Lat 
#   - Lon 
#   - BP 
#   - Ts
#
#------------------------------------------------------------------------------

iabp_to_csv <- function(raw_data) {
  
  # Manufacturer sensor range values
  #---------------------------------

  # Debug
  message("Executing script: iabp_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "IABP"
  
  # 3) Data timestamp (UTC)
  if("Date" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$Date, truncated = 2)
  } else {
    datetime_data <- NA
  }

  # 5) Latitude
  if("Lat" %in% names(raw_data)) {
    latitude <- raw_data$Lat
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("Lon" %in% names(raw_data)) {
    longitude <- raw_data$Lon
  } else {
    longitude <- NA
  }
  
  # 10) Surface temperature
  if("Ts" %in% names(raw_data)) {
    ts <- raw_data$Ts
  } else {
    ts <- NA
  }
  
  # 11) Barometric pressure
  if("BP" %in% names(raw_data)) {
    bp <- (raw_data$BP)
  } else {
    bp <- NA
  }

  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                               latitude, longitude, ts, bp, row.names=NULL)
  
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