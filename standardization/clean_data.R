#------------------------------------------------------------------------------
# Title: Standardized beacon data cleaning
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
#   - Clean raw data by removing invalid data.
#   - This function is intended to improve the efficiency of data 
#   standardization by removing duplicate code shared between all script.
#
# Required file(s): 
#   - Standardized data variable, which is returned by the function.
#
# Notes: 
#   - Assign NA to values exceeding sensor ranges but retain rows for location 
#   data.
#   - Data is cleaned using default or manufacturer sensor range values
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
#------------------------------------------------------------------------------

clean_data <- function(processed_data) {
  
  # Debug
  message("Executing script: clean_data.R")
  
  # 2) Remove invalid data datetimes
  processed_data <- processed_data[complete.cases(processed_data[ , 3]),]
  
  # 5) Remove latitudes > 90 degrees
  if("latitude" %in% names(processed_data)) {
    processed_data <- processed_data[which(processed_data$latitude <= latitude_max & processed_data$latitude >= latitude_min),]
  }
  
  # 6) Remove longitudes >180 or <-180 degrees
  if("longitude" %in% names(processed_data)) {
    processed_data <- processed_data[which(processed_data$longitude <= longitude_max & processed_data$longitude >= longitude_min),]
  }
  
  # 7) Battery voltage
  if("vbat" %in% names(processed_data)) {
    processed_data$vbat[processed_data$vbat > vbat_max | processed_data$vbat < vbat_min] <- NA
  }
  
  # 8) Air temperature
  if("ta" %in% names(processed_data)) {
    processed_data$ta[processed_data$ta > ta_max | processed_data$ta < ta_min] <- NA
  }
  
  # 9) Internal temperature
  if("ti" %in% names(processed_data)) {
    processed_data$ti[processed_data$ti > ti_max | processed_data$ti < ti_min] <- NA
  }
  
  # 10) Surface temperature
  if("ts" %in% names(processed_data)) {
    processed_data$ts[processed_data$ts > ts_max | processed_data$ts < ts_min] <- NA
  }
  
  # 11) Barometric Pressure
  if("bp" %in% names(processed_data)) {
    processed_data$bp[processed_data$bp > bp_max | processed_data$bp < bp_min] <- NA
  }
  
  # 12) Pitch
  if("pitch" %in% names(processed_data)) {
    processed_data$pitch[processed_data$pitch > pitch_max | processed_data$pitch < pitch_min] <- NA
  }
  
  # 13) Roll
  if("roll" %in% names(processed_data)) {
    processed_data$roll[processed_data$roll > roll_max | processed_data$roll < roll_min] <- NA
  }
  
  # 14) Heading
  if("heading" %in% names(processed_data)) {
    processed_data$heading[processed_data$heading > heading_max | processed_data$heading < heading_min] <- NA
  }
  
  # 15) Satellites
  if("satellites" %in% names(processed_data)) {
    processed_data$satellites[processed_data$satellites > satellites_max | processed_data$satellites < satellites_min] <- NA
  }
  
  # 16) Location accuracy (Argos)
  if("loc_accuarcy" %in% names(processed_data)) {
    processed_data$loc_accuarcy[processed_data$loc_accuarcy > loc_accuarcy_max | processed_data$loc_accuarcy < loc_accuarcy_min] <- NA
  }
  
  # 17) Message index (Arcgos)
  if("message_index" %in% names(processed_data)) {
    processed_data$message_index[processed_data$message_index > message_index_max | processed_data$message_index < message_index_min] <- NA
  }
  
  # 18) GPS delay
  if("gps_delay" %in% names(processed_data)) {
    processed_data$gps_delay[processed_data$gps_delay > gps_delay_max | processed_data$gps_delay < gps_delay_min] <- NA
  }
  
  # 19) SNR
  if("snr" %in% names(processed_data)) {
    processed_data$snr[processed_data$snr > snr_max | processed_data$snr < snr_min] <- NA
  }
  
  # 20) TTFF
  if("ttff" %in% names(processed_data)) {
    processed_data$heading[processed_data$ttff > ttff_max | processed_data$ttff < ttff_min] <- NA
  }
  
  # Assign data to new variable name
  cleaned_data <- processed_data
  
  # Order the data chronologically
  cleaned_data <- cleaned_data[order(cleaned_data$datetime_data), ]
  
  # Omit duplicate records with same timestamp using 'distinct' from dplyr. 
  # Note: This only retains first instance.
  cleaned_data <- distinct(cleaned_data, datetime_data, .keep_all=TRUE)
  
  return(cleaned_data)

} # End function