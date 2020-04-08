#------------------------------------------------------------------------------
# Title: Validation of standardized csv format
#
# Created by: Anna Crawford, Carleton University
#
# Date: March 6, 2014
#
# Modified by: 
#   Adam Garbo, Carleton University, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Descrition: 
#   Assertion function to make sure that the processed CSV data is standardized
#   correctly before feeding through quality added functions
#
# Necessary file(s): 
#   Processed csv beacon data written to 'output' directory with 
#   beacon_processing.R
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
#------------------------------------------------------------------------------

# Note - can this be fixed so that you can tell which line was wrong? 
validation <- function(standard_data) {
  
  # Debug
  message("Executing script: validation.R")
  
  # Validate that all columns are filled with appropriate data types
  if("beacon_id" %in% names(standard_data)) {
    validate_that(standard_data$beacon_id[1] == filename)
  }
  if("beacon_type" %in% names(standard_data)) {
    validate_that(is.character(standard_data[2:length(standard_data$beacon_type),2]))
  }
  if("datetime_data" %in% names(standard_data)) {
    validate_that(is.POSIXct(standard_data[2:length(standard_data$datetime_data),3]))
  }
  if("datetime_transmit" %in% names(standard_data)) {
    validate_that(is.POSIXct(standard_data[2:length(standard_data$datetime_transmit),4]))
  }
  if("latitude" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$latitude),5]))
  }
  if("longitude" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$longitude),6]))
  }
  if("vbat" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$vbat),7]))
  }
  if("ta" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$ta),8]))
  }
  if("ti" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$ti),9]))
  }
  if("ts" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$ts),10]))
  }
  if("bp" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$bp),11]))
  }          
  if("pitch" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$pitch),12]))
  }            
  if("roll" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$roll),13]))
  }              
  if("heading" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$heading),14]))
  }                
  if("satellites" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$satellites),15]))
  }
  if("loc_accuracy" %in% names(standard_data)) {
    validate_that(is.integer(standard_data[2:length(standard_data$loc_accuracy),16]))
  }
  if("message_index" %in% names(standard_data)) {
    validate_that(is.integer(standard_data[2:length(standard_data$message_index),17]))
  }
  if("gps_delay" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$gps_delay),18]))
  }
  if("snr" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$snr),19]))
  }
  if("ttff" %in% names(standard_data)) {
    validate_that(is.numeric(standard_data[2:length(standard_data$ttff),20]))
  }
} # End function