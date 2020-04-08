#------------------------------------------------------------------------------
# Title: Standardization of iceberg tracking beacon data
#
# Created by: Adam Garbo
#
# Date: June 15, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   Standardizes columns  
#   standardized CSV.
#   
# Required file(s): 
#   Standardized data CSV
#
# Notes: 
#
#   Standardized CSV columns 
#   -------------------------
#   1) beacon_id
#   2) beacon_type
#   3) datetime_data
#   4) datetime_transmit
#   5) latitude
#   6) longitude
#   7) vbat
#   8) ta
#   9) ti
#   10) ts
#   11) bp
#   12) pitch
#   13) roll
#   14) heading
#   15) satellites
#   16) loc_accuarcy
#   17) message_index
#   18) gps_delay
#   19) snr
#   20) ttff
#
#   Missing data columns are filled with NAs.
#
#------------------------------------------------------------------------------

standardize_columns <- function(cleaned_data) {
  
  # Debug
  message("Executing script: standardize_columns.R")
  
  # 1) Unique beacon identifier
  
  # 2) Beacon type
  
  # 3) Data timestamp (UTC)
  if(!("datetime_data" %in% names(cleaned_data))) {
    cleaned_data$datetime_data <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  if(!("datetime_transmit" %in% names(cleaned_data))) {
    cleaned_data$datetime_transmit <- NA
  }
  
  # 5) Latitude
  if(!("latitude" %in% names(cleaned_data))) {
    cleaned_data$latitude <- NA
  }
  
  # 6) Longitude
  if(!("longitude" %in% names(cleaned_data))) {
    cleaned_data$longitude <- NA
  }
  
  # 7) Battery voltage
  if(!("vbat" %in% names(cleaned_data))) {
    cleaned_data$vbat <- NA
  }
  
  # 8) Air temperature
  if(!("ta" %in% names(cleaned_data))) {
    cleaned_data$ta <- NA
  }
  
  # 9) Internal temperature
  if(!("ti" %in% names(cleaned_data))) {
    cleaned_data$ti <- NA
  }
  
  # 10) Surface temperature
  if(!("ts" %in% names(cleaned_data))) {
    cleaned_data$ts <- NA
  }
  
  # 11) Barometric pressure
  if(!("bp" %in% names(cleaned_data))) {
    cleaned_data$bp <- NA
  }
  
  # 12) Pitch
  if(!("pitch" %in% names(cleaned_data))) {
    cleaned_data$pitch <- NA
  }
  
  # 13) Roll
  if(!("roll" %in% names(cleaned_data))) {
    cleaned_data$roll <- NA
  }
  
  # 14) Tilt-compensated heading
  if(!("heading" %in% names(cleaned_data))) {
    cleaned_data$heading <- NA
  }
  
  # 15) GPS Satellites (Canatec beacons)
  if(!("satellites" %in% names(cleaned_data))) {
    cleaned_data$satellites <- NA
  }
  
  # 16) Location accuracy (Argos beacons)
  if(!("loc_accuracy" %in% names(cleaned_data))) {
    cleaned_data$loc_accuracy <- NA
  }
  
  # 17) Message Index (Argos beacons)
  if(!("message_index" %in% names(cleaned_data))) {
    cleaned_data$message_index <- NA
  }
  
  # 18) GPS delay
  if(!("gps_delay" %in% names(cleaned_data))) {
    cleaned_data$gps_delay <- NA
  }
  
  # 19) SNR
  if(!("snr" %in% names(cleaned_data))) {
    cleaned_data$snr <- NA
  }
  
  # 20) Time to first fix
  if(!("ttff" %in% names(cleaned_data))) {
    cleaned_data$ttff <- NA
  }
  
  # Assign data to new variable name
  standardized_data <- cleaned_data
  
  # Rearrange columns
  standardized_data <- standardized_data[c("beacon_id", 
                                   "beacon_type", 
                                   "datetime_data", 
                                   "datetime_transmit", 
                                   "latitude", 
                                   "longitude", 
                                   "vbat", 
                                   "ta", 
                                   "ti", 
                                   "ts", 
                                   "bp", 
                                   "pitch",
                                   "roll", 
                                   "heading", 
                                   "satellites", 
                                   "loc_accuracy", 
                                   "message_index", 
                                   "gps_delay", 
                                   "snr", 
                                   "ttff")]

  # Return data with standardized columns before writing CSV
  return(standardized_data)
  
} # End function
