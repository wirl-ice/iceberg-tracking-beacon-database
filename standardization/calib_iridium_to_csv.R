#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Author: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, Carleton University, April 26, 2018
#   Adam Garbo, Carleton University, June 19, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert a csv of raw MetOcean CALIB beacon with Iridium 
#   transmitter (iCALIB) to a standardized CSV before further processing to 
#   'quality added' files. 
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV)
#
# Notes: 
#   - This data should be kept in an 'input' directory with other raw data.
#   - iCALIBs have pressure and internal temperature sensors
#   - Ranges for sensor values are from CALIB Deployment and Operating Manual 
#   Job 695A
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - Asset Name
#   - Asset Id
#   - Data Date (UTC)
#   - Received Date (UTC)
#   - LATITUDE
#   - LONGITUDE
#   - FMTID
#   - YEAR
#   - MONTH
#   - DAY
#   - HOUR
#   - MIN
#   - SST
#   - BP
#   - BPT
#   - VBAT
#   - GPSDELAY
#   - SNR
#   - TTFF
#   - SBDTIME
#   - Report Body
#------------------------------------------------------------------------------

calib_iridium_to_csv <- function(raw_data) {

  # Manufacturer sensor range values
  #---------------------------------
  vbat_max = 18.6
  vbat_min = 6.0
  ta_max = 26.5
  ta_min = -50.0
  bp_max = 1074.6
  bp_min = 920.0
  
  # Debug message
  message("Executing script: calib_iridium_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "CALIB_IRIDIUM"
  
  # 3) Data timestamp (UTC)
  if("DATA.DATE..UTC." %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$DATA.DATE..UTC., truncated = 2)
  } else if ("Data.Date..UTC." %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$Data.Date..UTC., truncated = 2)
  } else {
    datetime_data <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  if("RECEIVED.DATE..UTC." %in% names(raw_data)) {
    datetime_transmit <- ymd_hms(raw_data$RECEIVED.DATE..UTC., truncated = 2)
  } else if ("Received.Date..UTC." %in% names(raw_data)) {
    datetime_transmit <- ymd_hms(raw_data$Received.Date..UTC., truncated = 2)
  } else {
    datetime_transmit <- NA
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
  if("VBAT" %in% names(raw_data)) {
    vbat <- raw_data$VBAT
  } else {
    vbat <- NA
  }
  
  # 10) Surface temperature
  if("SST" %in% names(raw_data)) {
    ts <- raw_data$SST
  } else {
    ts <- NA
  }
  
  # 11) Barometric pressure
  if("BP" %in% names(raw_data)) {
    bp <- (raw_data$BP)
  } else {
    bp <- NA
  }
  
  # 18) GPS delay
  if("GPSDELAY" %in% names(raw_data)) {
    gps_delay <- raw_data$GPSDELAY
  } else {
    gps_delay <- NA
  }
  
  # 19) SNR
  if("SNR" %in% names(raw_data)) {
    snr <- raw_data$SNR
  } else {
    snr <- NA
  }
  
  # 20) Time to first fix
  if("TTFF" %in% names(raw_data)) {
    ttff <- raw_data$TTFF
  } else {
    ttff <- NA
  }
  
  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                              datetime_transmit, latitude, longitude, vbat,
                              ts, bp, gps_delay, snr, ttff, row.names=NULL)
  
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