#------------------------------------------------------------------------------
# Title: Processing of raw beacon data
#
# Created by: Anna Crawford, Carleton University
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, July 24, 2015
#   Adam Garbo, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to choose which polar plot type to call. Usually done by 'day' 
#   unit of length. Only beacon type which is an exception is PPP, which 
#   normally does not operate for >24 hrs. The latter thus uses a minute unit 
#   of length.
#   - Called on by beacon_processing.R
#
# Required file(s):
#
#------------------------------------------------------------------------------

polar_plot_type <- function(beacon_type, standard_data) {
  
  # Debug
  message("Executing script: polar_plot_type.R")
  
  if (beaconType == "BIO") {
    pre_polar_plot(standard_data)   
  } else if (beaconType == "CALIB_ARGOS") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "CALIB_IRIDIUM") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "CANATEC") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "CCGS") {
    pre_polar_plot(standard_data)    
  } else if (beaconType == "CRYOLOGGER") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "GNSS") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "HEMISPHERE") {
    pre_polar_plot(standard_data)    
  } else if (beaconType == "IABP") {
    pre_polar_plot(standard_data) 
  } else if (beaconType == "NAVIDATUM") {
    pre_polar_plot(standard_data)    
  } else if (beaconType == "OCEANETIC") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "PPP") {
    pre_polar_plot(standard_data)  
  } else if (beaconType == "ROCKSTAR") {
    pre_polar_plot(standard_data)   
  } else if (beaconType == "SBD") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "SOLARA") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "SVP-I-BXGSA-L-AD") {
    pre_polar_plot(standard_data)
  } else if (beaconType == "SVP-I-BXGS-LP") {
    pre_polar_plot(standard_data)
  }  else if (beaconType == "SVP-I-XXGS-LP") {
    pre_polar_plot(standard_data)
  } # End if statement  
  
} # End function