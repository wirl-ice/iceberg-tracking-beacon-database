#------------------------------------------------------------------------------
# Title: 
#
# Created by: Adam Garbo
#
# Date: June 15, 2019
#
# Modified by: 
#   
#
# Project: 
#
# Description: 
#   
#   
# Required file(s): 
#   
#
# Notes: 
#   
#   Raw data columns (case sensitive):
#   ----------------------------------
#
#------------------------------------------------------------------------------

template <- function(raw_data) {
  
  # Debug
  message("Executing script: template")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "XXXXXXX"
  
  # 3) Data timestamp (UTC)
  if("XXXX" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$XXXX, truncated = 2)
  } else {
    datetime_data <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  if("XXXX" %in% names(raw_data)) {
    datetime_transmit <- ymd_hms(raw_data$XXXX, truncated = 2)
  } else {
    datetime_transmit <- NA
  }
  
  # 5) Latitude
  if("LATITUDE" %in% names(raw_data)) {
    latitude <- raw_data$latitude
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("LONGITUDE" %in% names(raw_data)) {
    longitude <- raw_data$longitude
  } else {
    longitude <- NA
  }
  
  # 7) Battery voltage
  if("VBAT" %in% names(raw_data)) {
    vbat <- raw_data$voltage
  } else {
    vbat <- NA
  }
  
  # 8) Air temperature
  if("AT" %in% names(raw_data)) {
    ta <- raw_data$AT
  } else {
    ta <- NA
  }
  
  # 9) Internal temperature
  if("TI" %in% names(raw_data)) {
    ti <- raw_data$temperature
  } else {
    ti <- NA
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
  
  # 15) GPS Satellites (Canatec beacons)
  if("SATELLITES" %in% names(raw_data)) {
    satellites <- raw_data$SATELLITES
  } else {
    satellites <- NA
  }
  
  # 16) Location accuracy (Argos beacons)
  if("LocAccuracy" %in% names(raw_data)) {
    loc_accuracy <- raw_data$LocAccuracy
  } else {
    loc_accuracy <- NA
  }
  
  # 17) Message Index (Argos beacons)
  if("MessageIndex" %in% names(raw_data)) {
    message_index <- raw_data$iterationCounter
  } else {
    message_index <- NA
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
  standard_data <- data.frame(beacon_id, beacon_type, datetime_data, datetime_transmit, 
                              latitude, longitude, vbat, ta, ti, ts, bp, pitch, roll, 
                              heading, satellites, loc_accuracy, message_index, gps_delay, 
                              snr, ttff, row.names=NULL)
  
  # Assign the column headings
  vars = c('BEACON_ID', 'BEACON_TYPE', 'DATETIME_DATA', 'DATETIME_TRANSMIT', 'LATITUDE', 
           'LONGITUDE', 'VBAT', 'TA', 'TI', 'TS', 'BP', 'PITCH', 'ROLL', 'HEADING', 
           'LOC_ACCURACY', 'MESSAGE_INDEX', 'SATELLITES', 'GPS_DELAY', 'SNR', 'TTFF')
  
  vars = c('beacon_id', 'beacon_type', 'datetime_data', 'datetime_transmit', 'latitude', 
           'longitude', 'vbat', 'ta', 'ti', 'ts', 'bp', 'pitch', 'roll', 'heading', 
           'satellites', 'loc_accuracy', 'message_index', 'gps_delay', 'snr', 'ttff')
  
  names(standard_data) = vars
  
  # Call data cleaning function
  clean_data(standard_data)
  
  # Choose file name
  output_file = filename
  
  # Choose output directory 
  setwd(output)
  write.csv(standard_data, paste(output_file, ".csv", sep = ""), row.names=FALSE)
  
} # End function