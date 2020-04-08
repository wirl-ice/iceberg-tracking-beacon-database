#------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Derek Mueller, Carleton University, 
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, March 6, 2014
#   Adam Garbo, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to write dataset to a point and line kml file, given a dataframe 
#   and output filename. 
#   - The dataframe must have columns lat + lon in wgs84
#   - Also only one beacon (line) per dataframe.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Standardized beacon CSV file
#------------------------------------------------------------------------------

csv_to_kml <- function(standard_data, filename) {
  
  # Debug
  message("Executing script: csv_to_kml.R")
  
  # Generate a field that is a unique identifier for each point (name of point) 
  name <- as.character(standard_data$beacon_id)
  obs <- seq(1,length(name))
  name <- paste(name, formatC(obs, flag = "0", width = 5), sep = "_")
  standard_data <- data.frame(name, standard_data)
  
  # Rename column
  names(standard_data)[names(standard_data) == 'name'] <- 'id'
  
  # Get matrix of coordinates - make line
  coords <- cbind(standard_data$longitude, standard_data$latitude)
  ln <- Line(coords)     # Make line, assume only one beacon - one line
  lns <- Lines(list(ln), ID = filename)    # Make this the only line in lines
  lnssp <- SpatialLines(list(lns))     # Turn into a spatial class
  proj4string(lnssp) <- CRS(c("+proj=longlat +ellps=WGS84"))     # Add the CRS
  
  lndf <- data.frame(filename)      # Make a dataframe to go with the line.
  names(lndf) <- 'BeaconID'      
  
  # Rownames have to match!
  row.names(lndf) <- sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
  
  # Link spatial lines with dataframe
  lnsspdf <- SpatialLinesDataFrame(lnssp, data = lndf)
  
  # Get coords, make points
  coordinates(standard_data) <- c("longitude", "latitude")
  
  # Define the projection - assume WGS84 lat lon
  proj4string(standard_data) <- CRS(c("+proj=longlat +ellps=WGS84"))
  
  # Overwrite all files - backup first if you want to save the files!
  delete_file(filename, c("kml"))
  
  # Write files
    
  # KML
  writeOGR(standard_data, paste(getwd(),paste(filename, "_pt", ".kml", sep = ""), 
                                sep = "/"), filename, "KML")
  writeOGR(lnsspdf, paste(getwd(),paste(filename, "_ln", ".kml", sep = ""), 
                          sep = "/"), filename, "KML")
  
  # Check that shape files were written
  if (!file_test("-f", paste(filename,"_pt",".kml", sep=''))) {
    stop("KML point file not created")
  } else {
    message("Check: KML point file created")
  }
  
  if (!file_test("-f", paste(filename,"_ln",".kml", sep=''))) {
    stop("KML line file not created")
  } else {
    message("Check: KML line file created")
  } # End check
  
} # End function