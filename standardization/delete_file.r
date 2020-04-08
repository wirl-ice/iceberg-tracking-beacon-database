#------------------------------------------------------------------------------
# Title: Deleting replicated files
#
# Created by: Derek Mueller, Carleton University
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, March 7, 2014
#   Adam Garbo, June 18, 2019
#
# Project: Ice island drift, deterioration and detection 
#
# Description: 
#   - Function to delete files already existing in processed beacon data 
#   directory which the script is attempting to write.
#   - Called on by beacon_processing.R
#------------------------------------------------------------------------------

delete_file = function(filename, ext) {
  
  # Debug
  message("Executing script: delete_file.R")
  
  for (i in 1:length(ext)) {
    if ( file.exists( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, "_pt.", ext[i], sep=""), sep="/") )
    }
    if ( file.exists( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, "_ln.", ext[i], sep=""), sep="/") )
    }
    if ( file.exists( paste(getwd(),paste(filename, ext[i], sep="."), sep="/") ) ) {
      file.remove( paste(getwd(),paste(filename, ext[i], sep="."), sep="/") )
    }
  } # End for loop
} # End function