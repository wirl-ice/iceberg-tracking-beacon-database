#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Adam Garbo
# 
# Date: July 5, 2019
#
# Based on work by: Anna Crawford, Carleton University, 2014
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw Canatec beacon data to a standardized CSV
#   before further processing to 'quality added' files. 
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Canatec raw beacon data (CSV)
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#   - Sensor range values are from Canatec Ice Drift Beacon model V3.1-THA-12 
#   Operation Manual
#
#   Raw data columns (case sensitive):
#   -----------------------------------
#   - ReadingDate
#   - Latitude
#   - Longitude
#   - Elevation
#   - Heading
#   - Speed
#   - Fix
#   - Satellites
#   - HDOP
#   - VDOP
#   - VerticalVelocity
#   - Pressure
#   - TempExternal
#   - TempInternal
#   - BeaconAlarmState
#   - BatteryVoltage
#   - ModemVoltage
#   - WindSpeed
#   - WindDirection
#
#------------------------------------------------------------------------------

canatec_to_csv <- function(raw_data) {
  
  # Manufacturer sensor range values
  
  # Debug
  message("Executing script: canatec_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "CANATEC"
  
  # 3) Data timestamp (UTC)
  # Canatec beacon time can be either: ymd_hms, ymd_hm or mdy_hms, mdy_hm. 
  # Code is now adaptive and will check for appropriate format and can handle 
  # missing seconds.
  
  # Beacons with mdy_hms datetime format:
  mdy_hms <- c("300034012571050_2009",
               "26973_2009", 
               "300034012592660_2010",
               "300234062791700_2015",
               "300234062792500_2015",
               "300234062795460_2015",
               "300234062795640_2015",
               "300234062796640_2015")
  # Beacons with ymd_hms or ymd_hm datetime format:
  ymd_hms <- c("300234011240410_2013",
               "300234011241410_2013",
               "300234011242410_2013",
               "300234011938510_2013",
               "300234062790480_2015",
               "300234062791420_2015",
               "300234062792490_2015",
               "300234062794470_2015",
               "300234062796490_2015")
  
  if("ReadingDate" %in% names(raw_data)) {
    if(filename %in% mdy_hms) {
      datetime_data <- mdy_hms(raw_data$ReadingDate, truncated = 2) 
    } else if(filename %in% ymd_hms) {
      datetime_data <- ymd_hms(raw_data$ReadingDate, truncated = 2) 
    } else {
      datetime_data <- NA
    }
  } else {
    datetime_data <- NA
  }
  
  # 5) Latitude
  if("Latitude" %in% names(raw_data)) {
    latitude <- raw_data$Latitude
  } else {
    latitude <- NA
  }
  
  # 6) Longitude
  if("Longitude" %in% names(raw_data)) {
    longitude <- raw_data$Longitude
  } else {
    longitude <- NA
  }
  
  # 7) Battery voltage
  if("BatteryVoltage" %in% names(raw_data)) {
    vbat <- raw_data$BatteryVoltage
  } else {
    vbat <- NA
  }
  
  # 8) Air temperature
  if("TempExternal" %in% names(raw_data)) {
    ta <- raw_data$TempExternal
  } else {
    ta <- NA
  }
  
  # 9) Internal temperature
  if("TempInternal" %in% names(raw_data)) {
    ti <- raw_data$TempInternal
  } else {
    ti <- NA
  }
  
  # 11) Barometric pressure
  if("Pressure" %in% names(raw_data)) {
    bp <- (raw_data$Pressure)
  } else {
    bp <- NA
  }
  
  # 15) GPS Satellites (Canatec beacons)
  if("Satellites" %in% names(raw_data)) {
    satellites <- raw_data$Satellites
  } else {
    satellites <- NA
  }
  
  # Put the declared variables into a single data frame
  processed_data <- data.frame(beacon_id, beacon_type, datetime_data, 
                              latitude, longitude, vbat, ta, ti, bp, 
                              satellites, row.names=NULL)
  
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