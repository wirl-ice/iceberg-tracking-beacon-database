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
# Project: Ice island drift, deterioration and detection 
#
# Description: 
#   - Function to write dataset to a point and line shapefile, given a dataframe 
#   and output filename. 
#   - The dataframe must have columns lat + lon in wgs84
#   - Also only one beacon (line) per dataframe.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Standardized beacon csv file
# 
# Notes: 
#   - Removed following line of code to use full IMEI instead 
#   "name <- substr(name,nchar(name)-5,nchar(name))"
#------------------------------------------------------------------------------

csv_to_shp <- function(standard_data, filename) {
  
  # Debug
  message("Executing script: csv_to_shp.R")
  
  # Create copy of dataset for manipulation
  shapefile_data <- standard_data
  
  # Generate a field that is a unique identifier for each point (name of point) 
  name <- as.character(shapefile_data$beacon_id)
  obs <- seq(1,length(name))
  name <- paste(name, formatC(obs, flag = "0", width = 5), sep = "_")
  shapefile_data <- data.frame(name, shapefile_data)
  
  # Rename column
  names(shapefile_data)[names(shapefile_data) == 'name'] <- 'id'
  
  # Get matrix of coordinates - make line
  coords <- cbind(shapefile_data$longitude, shapefile_data$latitude)
  
  # Make line, assume only one beacon - one line
  ln <- Line(coords)
  
  # Make this the only line in lines
  lns <- Lines(list(ln), ID = filename)    
  
  # Turn into a spatial class
  lnssp <- SpatialLines(list(lns))    
  
  # Add the CRS
  proj4string(lnssp) <- CRS(c("+proj=longlat +datum=WGS84"))     
  
  # Make a dataframe to go with the line...
  lndf <- data.frame(filename)      
  
  # Assign column name
  names(lndf) <- 'beacon_id'      
  
  # Rownames have to match!
  row.names(lndf) <- sapply(slot(lnssp, "lines"), function(x) slot(x, "ID"))
  
  # Link spatial lines with dataframe
  lnsspdf <- SpatialLinesDataFrame(lnssp, data = lndf)
  
  # Get coords, make points
  coordinates(shapefile_data) <- c("longitude", "latitude")
  
  # Define the projection - assume WGS84 lat lon
  proj4string(shapefile_data) <- CRS(c("+proj=longlat +datum=WGS84"))
  
  # Overwrite all files - backup first if you want to save the files!
  delete_file(filename, c("shp","dbf","prj","shx"))

  # Write files:
  setwd(output)
  
  # Create point shapefile
  writeOGR(shapefile_data, getwd(), paste(filename, "_pt", sep = ""), 
           driver = 'ESRI Shapefile')
  
  # Create line shapefile
  writeOGR(lnsspdf, getwd(), paste(filename, "_ln", sep = ""), 
           driver = 'ESRI Shapefile')
  
  # Check that shape files were written
  if (!file_test("-f", paste(filename,"_pt",".shp", sep=''))) {
    stop("Point shapefile file not created")
  } else {
    message("Check: Point shapefile file created")
  }
  
  if (!file_test("-f", paste(filename,"_ln",".shp", sep=''))) {
    stop("Line shapefile not created")
  } else {
    message("Check: Line shapefile file created")
  } # End check
  
} # End function