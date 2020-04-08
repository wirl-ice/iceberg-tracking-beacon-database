#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Adam Garbo
#
# Date: June 17, 2019
#
# Modified by: 
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw CSV data from Cryologger ice tracking beacons to
#   standardized CSV before further processing to 'quality added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Cryologger raw beacon data (CSV)
#
# Notes: 
#   - Cryologger beacon data should be separated into separate CSV files.
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - imei
#   - momsn 
#   - transmit_time 
#   - iridium_latitude 
#   - iridium_longitude 
#   - iridium_cep 
#   - data 
#   - unixtime 
#   - temperature, 
#   - pressure 
#   - pitch 
#   - roll 
#   - heading 
#   - latitude 
#   - longitude 
#   - satellites 
#   - hdop 
#   - voltage 
#   - transmitDuration 
#   - iterationCounter
#
#------------------------------------------------------------------------------

cryologger_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: cryologger_to_csv.R")
  
  # Manufacturer sensor range values
  #---------------------------------
  vbat_max = 8.0
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "CRYOLOGGER"
  
  # 3) Data timestamp (UTC)
  if("unixtime" %in% names(raw_data)) {
    datetime_data <- anytime(raw_data$unixtime, tz="UTC")
  } else {
    datetime_data <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  if("transmit_time" %in% names(raw_data)) {
    datetime_transmit <- ymd_hms(raw_data$transmit_time, truncated = 2)
  } else {
    datetime_transmit <- NA
  }
  
  # 5) Latitude
  if("latitude" %in% names(raw_data)) {
    latitude <- raw_data$latitude
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("longitude" %in% names(raw_data)) {
    longitude <- raw_data$longitude
  } else {
    longitude <- NA
  }
  
  # 7) Battery voltage
  if("voltage" %in% names(raw_data)) {
    vbat <- raw_data$voltage
  } else {
    vbat <- NA
  }
  
  # 9) Internal temperature
  if("temperature" %in% names(raw_data)) {
    ti <- raw_data$temperature
  } else {
    ti <- NA
  }
  
  # 11) Barometric pressure
  if("pressure" %in% names(raw_data)) {
    raw_data$pressure <- raw_data$pressure * 10.0 # Conver to hPa
    bp <- (raw_data$pressure)
  } else {
    bp <- NA
  }
  
  # 12) Pitch
  if("pitch" %in% names(raw_data)) {
    pitch <- raw_data$pitch
  } else {
    pitch <- NA
  }
  
  # 13) Roll
  if("roll" %in% names(raw_data)) {
    roll <- raw_data$roll
  } else {
    roll <- NA
  }
  
  # 14) Tilt-compensated heading
  if("heading" %in% names(raw_data)) {
    heading <- raw_data$heading
  } else {
    heading <- NA
  }
  
  # 15) GPS Satellites
  if("satellites" %in% names(raw_data)) {
    satellites <- raw_data$satellites
  } else {
    satellites <- NA
  }
  
  # Put the declared variables into one data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, datetime_transmit, 
                              latitude, longitude, vbat, ti, bp, pitch, roll, 
                              heading, satellites, row.names=NULL)

  
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