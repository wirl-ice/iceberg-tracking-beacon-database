#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2014
#
# Modified by: 
#   Jill Rajewicz, April 26, 2018
#   Adam Garbo, June 17, 2018
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert raw data downloaded from the International Arctic 
#   Buoy Program website to a standardized csv before further processing to 
#   'quality added' files.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Raw beacon data (CSV)
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#   - Note: Beacon type has to be checked and filled in - this just deals with 
#   format of data from IABPrepository (but many different beacon types report 
#   there).
#
#  Raw data columns (case sensitive):
# -----------------------------------
#   - BuoyID
#   - Year 
#   - Hour 
#   - Min 
#   - DOY 
#   - POS_DOY 
#   - Lat 
#   - Lon 
#   - BP 
#   - Ts
#
#------------------------------------------------------------------------------

iabp_to_csv <- function(raw_data) {
  
  # Manufacturer sensor range values
  #---------------------------------
  
  # Debug
  message("Executing script: iabp_to_csv.R")
  
  # 1) Unique beacon identifier
  beacon_id <- filename
  
  # 2) Beacon type
  beacon_type <- "IABP"
  
  
  
  
  