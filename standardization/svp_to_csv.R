#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, April 26, 2018
#   Adam Garbo, June 18, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw MetOcean SVP buoy data to a standardized CSV 
#   before further processing to 'quality added' files.
#   Supported models:
#   1) SVP-I-XXGS-P
#   2) SVP-I-XXGS-LP
#   3) SVP-I-BXGS-P
#   4) SVP-I-BXGSA-L-AD
#   - Note: Other MetOcean Polar SVP models are likely also supported.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV)
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#   - BXGS-P buoys have BP, GPS, and SST
#   - SVP-XXGS-P beacons have GPS and SST, no BP
#   - SVP-I-BXGSA-L-AD buoys have BP, GPS, SST, AT, lithium battery and are 
#   designed for air deployment in Arctic regions
#   - Sensor range values are from iSVP technical manual v1.5 and
#     SVP-I-BXGSA-L-AD Deployment and User's Manual Version 1.0
#
#   Raw data columns (case sensitive):
#   ----------------------------------
#   - Asset Name / Asset.Name
#   - Asset Id  / Asset.Id
#   - Data Date (UTC) / DataDate_UTC
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
#   - AT
#   - VBAT
#   - GPSDELAY
#   - SNR
#   - TTFF
#   - SBDTIME
#   - Report Body / Report.Body
#
#------------------------------------------------------------------------------

svp_to_csv <- function(raw_data) {
  
  # Debug
  message("Executing script: svp_to_csv.R")
  
  # Manufacturer sensor range values
  #---------------------------------
  
  if(beacon_type == "SVP-I-BXGSA-L-AD") {
    vbat_max = 17.6
    vbat_min = 5.0
    bp_max = 1054.7
    bp_min = 850
    sst_max = 42.3
    sst_min = -60
    ta_max = 42.3
    ta_min = -60.0
  
  } else if(beacon_type == "SVP-I-BXGS-LP") {
    vbat_max = 17.6
    vbat_min = 5.0
    bp_max = 1054.7
    bp_min = 850
    sst_max = 42.3
    sst_min = -60
    ta_max = 42.3
    ta_min = -60.0
    
  } else if(beacon_type == "SVP-I-XXGS-LP") {
    vbat_max = 17.6
    vbat_min = 5.0
    bp_max = 1054.7
    bp_min = 850
    sst_max = 42.3
    sst_min = -60
    ta_max = 42.3
    ta_min = -60.0
  }
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "SVP"
  
  # 3) Data timestamp (UTC)
  # Note: Account for observed variations of column names
  if("DATA.DATE..UTC." %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$DATA.DATE..UTC., truncated = 2)
  } else if("Data.Date..UTC." %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$Data.Date..UTC., truncated = 2)
  } else if("DataDate_UTC" %in% names(raw_data)) {
    datetime_data <- ymd_hms(raw_data$DataDate_UTC, truncated = 2)
  } else {
    datetime_data <- NA
  }
  
  # 4) Data transmission timestamp (UTC)
  # Note: Account for observed variations of column names
  if("RECEIVED.DATE..UTC." %in% names(raw_data)) {
    datetime_transmit <- ymd_hms(raw_data$RECEIVED.DATE..UTC., truncated = 2)
  } else if("Received.Date..UTC." %in% names(raw_data)) {
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
  
  # 8) Air temperature
  if("AT" %in% names(raw_data)) {
    ta <- raw_data$AT
  } else {
    ta <- NA
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
                               datetime_transmit, latitude, longitude, 
                               vbat, ta, ts, bp, gps_delay, snr, ttff, 
                               row.names=NULL)
  
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