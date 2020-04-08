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
#   - Function to convert a csv of beacon data messages generated using 
#   Download_and_Decode.py translate beacon data send as sbd messages to 
#   Gmail into a CSV into a standardized csv before further processing to 
#   'quality added' files. 
#   Called on by beacon_processing.R  
#
# Necessary file(s): 
#   - Raw data csv from emailed SBD files downloaded and cleaned by Derek 
#   Mueller's 'download & decode' Python script
#
# Notes:
#   - Raw data should be kept in an 'input' directory with other raw data.
#   - Note: SBD files can be from different beacon types, so script must be 
#   altered to include correct beacon type
#------------------------------------------------------------------------------

sbd_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: sbd_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "SBD" # ENTER CORRECT BEACON TYPE
  
  # 3) Data timestamp (UTC)
  #if("XXXX" %in% names(raw_data)) {
  #  datetime_data <- ymd_hms(raw_data$XXXX, truncated = 2)
  #} else {
  #  datetime_data <- NA
  #}
  date <- as.Date(paste(raw_data$Year, raw_data$Month, raw_data$Day, sep = "-"), "%Y-%m-%d")
  time <- paste(raw_data$Hour, raw_data$Minute, "00", sep=":")
  datetime_data <- as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz='UTC')
  
  # 5) Latitude
  if("Latitude" %in% names(raw_data)) {
    latitude <- raw_data$Latitude
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("Longitude" %in% names(raw_data)) {
    longitude <- raw_data$Longitude
    longitude <- ifelse(raw_data$longitude > 180, -360 + raw_data$longitude, raw_data$longitude)
  } else {
    longitude <- NA
  }
  
  # 7) Battery voltage
  if("Voltage.Battery" %in% names(raw_data)) {
    vbat <- raw_data$Voltage.Battery
  } else {
    vbat <- NA
  }

  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, latitude,
                              longitude, vbat, row.names=NULL)
  
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