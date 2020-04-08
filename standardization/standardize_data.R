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
#   Determines approproiate script to execute to convert raw data to 
#   standardized CSV.
#   Provides default sensor range values for data cleaning, which are
#   overwritten by manufacturer values if specified in each function.
#   
# Required file(s): 
#   Raw data
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
#   Missing sensors/info are set to NA.
#   Data may not be available from all beacon types to complete standardized 
#   CSV columns
#
#------------------------------------------------------------------------------

standardize_data <- function(beacon_type, raw_data) {
  
  # Debug
  message("Executing script: standardize_data.R")
  
  # Determine processing script to execute
  if (beacon_type == "BIO") {
    bio_to_csv(raw_data)
  } else if (beacon_type == "CALIB_ARGOS") {
    calib_argos_to_csv(raw_data)
  } else if (beacon_type == "CALIB_IRIDIUM") {
    calib_iridium_to_csv(raw_data)    
  } else if (beacon_type == "CANATEC") {
    canatec_to_csv(raw_data)    
  } else if (beacon_type == "CCGS") {
    ccgs_to_csv(raw_data)    
  } else if (beacon_type == "CRYOLOGGER") {
    cryologger_to_csv(raw_data)
  } else if (beacon_type == "GNSS") {
    gnss_to_csv(raw_data)    
  } else if (beacon_type == "IABP") {
    iabp_to_csv(raw_data)    
  } else if (beacon_type == "NAVIDATUM") {
    navidatum_to_csv(raw_data)    
  } else if (beacon_type == "OCEANETIC") {
    oceanetic_to_csv(raw_data)
  } else if (beacon_type == "PPP") {
    ppp_to_csv(raw_data)    
  } else if (beacon_type == "ROCKSTAR") {
    rockstar_to_csv(raw_data)    
  } else if (beacon_type == "SBD") {
    sbd_to_csv(raw_data)    
  } else if (beacon_type == "SOLARA") {
    solara_to_csv(raw_data)
  } else if (beacon_type == "SVP-I-BXGSA-L-AD") {
    svp_to_csv(raw_data)
  } else if (beacon_type == "SVP-I-BXGS-LP") {
    svp_to_csv(raw_data)
  }  else if (beacon_type == "SVP-I-XXGS-LP") {
    svp_to_csv(raw_data)
  } else {
    stop("Beacon type not recognized")
  } # End if statement 
  
  # Check if CSV was written
  if (!file_test("-f", paste(filename,".csv",sep=''))) {
    stop("CSV file not written")
  } else {
    message("Check: CSV file written")
  } # End check
  
} # End function

