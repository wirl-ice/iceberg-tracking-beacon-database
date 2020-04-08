#--------------------------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Adam Garbo, March 16, 2019
#
# Modified by: 
#   Adam Garbo, March 21, 2019
# 
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   Function to convert raw CSV data from Cryologger ice tracking beacons to a
#   standardized csv before further processing to 'quality added' files.
#   Called on by BeaconProcessing.R
#   
# Required file(s): 
#   Cryologger raw beacon data (CSV)
#
# Notes: 
#   Raw data should be kept in an 'input' directory with other raw data.
#   Cryologger data should be separated into separate CSV files.
#--------------------------------------------------------------------------------------------------

cryologger_to_csv <- function(raw_data) {
  
  # Debug
  message("Script: cryologger_to_csv")
  
  # Cryologger raw data columns (case sensitive):
  #---------------------------------------------
  # 1) imei 
  # 2) momsn 
  # 3) transmit_time 
  # 4) iridium_latitude 
  # 5) iridium_longitude 
  # 6) iridium_cep 
  # 7) data 
  # 8) unixtime 
  # 9) temperature, 
  # 10) pressure 
  # 11) pitch 
  # 12) roll 
  # 13) heading 
  # 14) latitude 
  # 15) longitude 
  # 16) satellites 
  # 17) hdop 
  # 18) voltage 
  # 19) transmitDuration 
  # 20) iterationCounter
  
  # Standardized CSV columns:
  #--------------------------
  # 1) BEACON_ID
  # 2) BEACON_TYPE
  # 3) DATETIME_DATA
  # 4) DATETIME_TRANSMIT
  # 5) LATITUDE
  # 6) LONGITUDE
  # 7) VBAT
  # 8) TA
  # 9) TI
  # 10) TS
  # 11) BP
  # 12) PITCH
  # 13) ROLL
  # 14) HEADING
  # 15) SATELLITES
  # 16) LOC_ACCURACY
  # 17) MEASSAGE_INDEX
  # 18) GPS_DELAY
  # 19) SNR
  # 20) TTFF

  # Note: Missing sensors/info are set to NA.
  # Data may not be available from all beacon types to complete standardized CSV columns
  
  # 1) Unique beacon identifier
  BEACON_ID <- filename
  
  # 2) Beacon type
  BEACON_TYPE <- "CRYOLOGGER"
  
  # 3) Data timestamp (UTC)
  if("unixtime" %in% names(raw_data)) {
    DATETIME_DATA <- anytime(raw_data$unixtime, tz="UTC")
  } else {
    DATETIME_DATA <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  if("transmit_time" %in% names(raw_data)) {
    DATETIME_TRANSMIT <- ymd_hms(raw_data$transmit_time, tz='UTC')
  } else {
    DATETIME_TRANSMIT <- NA
  }
  
  # 5) Latitude
  if("latitude" %in% names(raw_data)) {
    LATITUDE <- raw_data$latitude # Latitude in decimal degrees (DD)
  } else {
    LATITUDE <- NA
  }

  # 6) Longitude
  if("longitude" %in% names(raw_data)) {
    LONGITUDE <- raw_data$longitude # Latitude in decimal degrees (DD)
  } else {
    LONGITUDE <- NA
  }

  # 7) Battery voltage
  if("voltage" %in% names(raw_data)) {
    VBAT <- raw_data$voltage
  } else {
    VBAT <- NA
  }
  
  # 8) Air temperature
  if("AT" %in% names(raw_data)) {
    TA <- raw_data$AT
  } else {
    TA <- NA
  }
  
  # 9) Internal temperature
  if("temperature" %in% names(raw_data)) {
    TI <- raw_data$temperature
  } else {
    TI <- NA
  }
  
  # 10) Surface temperature
  if("SST" %in% names(raw_data)) {
    TS <- raw_data$SST
  } else {
    TS <- NA
  }
  
  # 11) Barometric pressure
  if("pressure" %in% names(raw_data)) {
    raw_data$pressure <- raw_data$pressure * 10.0 # Conver to hPa
    BP <- (raw_data$pressure)
  } else {
    BP <- NA
  }
  
  # 12) Pitch
  if("pitch" %in% names(raw_data)) {
    PITCH <- raw_data$pitch
  } else {
    PITCH <- NA
  }
  
  # 13) Roll
  if("roll" %in% names(raw_data)) {
    ROLL <- raw_data$roll
  } else {
    ROLL <- NA
  }
  
  # 14) Tilt-compensated heading
  if("heading" %in% names(raw_data)) {
    HEADING <- raw_data$heading
  } else {
    HEADING <- NA
  }
  
  # 15) GPS Satellites (Canatec beacons)
  if("satellites" %in% names(raw_data)) {
    SATELLITES <- raw_data$satellites
  } else {
    SATELLITES <- NA
  }
  
  # 16) Location accuracy (Argos beacons)
  if("LocAccuracy" %in% names(raw_data)) {
    LOC_ACCURACY <- raw_data$LocAccuracy
  } else {
    LOC_ACCURACY <- NA
  }
  
  # 17) Message Index (Argos beacons)
  if("iterationCounter" %in% names(raw_data)) {
    MESSAGE_INDEX <- raw_data$iterationCounter
  } else {
    MESSAGE_INDEX <- NA
  }
  
  # 18) GPS delay
  if("GPSDELAY" %in% names(raw_data)) {
    GPS_DELAY <- raw_data$GPSDELAY
  } else {
    GPS_DELAY <- NA
  }
  
  # 19) SNR
  if("SNR" %in% names(raw_data)) {
    SNR <- raw_data$SNR
  } else {
    SNR <- NA
  }
  
  # 20) Time to first fix
  if("TTFF" %in% names(raw_data)) {
    TTFF <- raw_data$TTFF
  } else {
    TTFF <- NA
  }
  
  # Put the declared variables into one data frame and 
  standard_data <- data.frame(BEACON_ID, BEACON_TYPE, DATETIME_DATA, DATETIME_TRANSMIT, 
                              LATITUDE, LONGITUDE, VBAT, TA, TI, TS, BP, PITCH, ROLL, 
                              HEADING, LOC_ACCURACY, MESSAGE_INDEX, SATELLITES, GPS_DELAY, 
                              SNR, TTFF, row.names=NULL)
  
  # Assign the column headings
  vars = c('BEACON_ID', 'BEACON_TYPE', 'DATETIME_DATA', 'DATETIME_TRANSMIT', 'LATITUDE', 
           'LONGITUDE', 'VBAT', 'TA', 'TI', 'TS', 'BP', 'PITCH', 'ROLL', 'HEADING', 
           'LOC_ACCURACY', 'MESSAGE_INDEX', 'SATELLITES', 'GPS_DELAY', 'SNR', 'TTFF')
  names(standard_data) = vars
  
  # Call data cleaning function
  clean_data(standard_data)
  
  # Choose file name
  output_file = filename
  
  # Choose output directory 
  setwd(output)
  write.csv(standard_data, paste(output_file, ".csv", sep = ""), row.names=FALSE)
  
} # End function