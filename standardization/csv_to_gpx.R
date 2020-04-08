#------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Derek Mueller, Carleton University
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
#   - Function to write dataset to a point and line gpx file given a dataframe
#   and output filename.  
#   - The dataframe must have columns lat + lon in wgs84
#   - Also only one beacon (line) per dataframe.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Standardized beacon CSV file
#------------------------------------------------------------------------------

csv_to_gpx <- function(standard_data, filename) {
  
  # Debug
  message("Executing script: csv_to_gpx.R")
  
  # Generate a field that is a unique identifier for each point (name of point)
  name <- as.character(standard_data$beacon_id)
  obs <- seq(1,length(name))
  name <- paste(name, formatC(obs, flag = "0", width = 5), sep = "_")
  standard_data <- data.frame(name, standard_data)
  
  # Rename column
  names(standard_data)[names(standard_data) == 'name'] <- 'id'
  
  # Get matrix of coordinates - make line
  coords <- cbind(standard_data$longitude, standard_data$latitude)
  
  # Make line, assume only one beacon - one line
  ln <- Line(coords)     
  
  # Make this the only line in lines
  lns <- Lines(list(ln), ID = filename)    
  
  # Turn into a spatial class
  lnssp <- SpatialLines(list(lns))
  
  # Add the coordinate reference system (CRS)
  proj4string(lnssp) <- CRS(c("+proj=longlat +ellps=WGS84"))   
  
  # Make a dataframe to go with the line
  lndf <- data.frame(filename)      
  names(lndf) <- 'beacon_id'      
  
  # Rownames have to match!
  row.names(lndf) <- sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
  
  # Link spatial lines with dataframe
  lnsspdf <- SpatialLinesDataFrame(lnssp, data = lndf)
  
  # Get coordinates, make points
  coordinates(standard_data) <- c("longitude", "latitude")
  
  # Define the projection - assume WGS84 lat lon
  proj4string(standard_data) <- CRS(c("+proj=longlat +ellps=WGS84"))
  
  # Overwrite all files - backup first if you want to save the files!
  delete_file(filename, c("gpx"))
  
  # Note: GPX doesn't work properly for tracks!
  
  # Write gpx files to output directory
  df <- standard_data[,c(1,5)]
  names(df) <- c('name','time')
  writeOGR(df, paste(getwd(),paste(filename, "_pt", ".gpx", sep = ""), 
                     sep = "/"), 'waypoints', "GPX")
  writeOGR(df, paste(getwd(),paste(filename, "_ln", ".gpx", sep = ""), 
                     sep = "/"), 'routes', "GPX")
  
  # Check that shape files were written
  if (!file_test("-f", paste(filename,"_pt",".gpx", sep=''))) {
    stop("GPX point file not created")
  } else {
    message("Check: GPX point file created")
  }
  
  if (!file_test("-f", paste(filename,"_ln",".gpx", sep=''))) {
    stop("GPX line file not created")
  } else {
    message("Check: GPX line file created")
  } # End check
  
} # End function