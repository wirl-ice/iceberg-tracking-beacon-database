#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, PhD Candidate DGES, Carleton University, 2014
#
# Modified by: 
#   Jill Rajewicz, April 26, 2018
#   Adam Garbo, June 18, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert a csv of raw data from an Oceanetic beacon to a 
#   standardized CSV before further processing to 'quality added' files. 
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Oceanetic raw beacon data (csv).
#
# Notes: 
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   IMEI
#   Year
#   Month
#   Day
#   Hour
#   Minute
#   Latitude
#   Longitude
#   Temperature
#   Voltage Battery
#   AtmPress
#   FormatID
#
#   *or*
#
#   beacon id
#   yr
#   mm
#   dd
#   hr
#   lat
#   long
#   
#   Note: Script will automatically process either raw data versions.
#------------------------------------------------------------------------------

oceanetic_to_csv <- function(raw_data) {
  
  # Debug message
  message("Executing script: oceanetic_to_csv.R")
  
  # Manufacturer sensor range values
  #---------------------------------
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "OCEANETIC"
  
  # 3) Data timestamp (UTC)
  if("Year" %in% names(raw_data)) {
    # Specific datetime format for beacon IDs: 3463170 and 0082470
    datetime_data <- with(raw_data, ymd_hms(paste(Year, Month, Day, Hour, Minute, sep= ' '), truncated = 2))
  } else if("yr" %in% names(raw_data)) {
    # Combine separate yr, mm, dd columns into one date object
    date <- ymd(paste(raw_data$yr, raw_data$mm, raw_data$dd, sep="-"))
    # Combine date and time to get POSIX date-time object
    datetime_data <- with(raw_data, as.POSIXct(paste(date, hr), format = "%Y-%m-%d %H", tz='UTC'))
    datetime_data <- ymd_hms(datetime_data, truncated = 2)
  } else {
    datetime_data <- NA
  }

  # 5) Latitude
  if("Latitude" %in% names(raw_data)) {
    latitude <- raw_data$Latitude
  } else if("lat" %in% names(raw_data)) {
    latitude <- raw_data$lat
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("Longitude" %in% names(raw_data)) {
    longitude <- raw_data$Longitude
  } else if("long" %in% names(raw_data)) {
    longitude <- raw_data$long
  } else {
    longitude <- NA
  }
  
  # 7) Battery voltage
  if("Voltage Battery" %in% names(raw_data)) {
    vbat <- raw_data$`Voltage Battery`
  } else {
    vbat <- NA
  }
  
  # 9) Internal temperature
  if("Temperature" %in% names(raw_data)) {
    ti <- raw_data$Temperature
  } else {
    ti <- NA
  }

  # 11) Barometric pressure
  if("AtmPress" %in% names(raw_data)) {
    bp <- (raw_data$AtmPress)
  } else {
    bp <- NA
  }
  
  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, latitude,
                              longitude, vbat, ti, bp, row.names=NULL)
  
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