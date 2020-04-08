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
#   - Function to convert standardized beacon csv to polarplot
#   - Called on by beacon_processing.R and pre_polar_plot.R
#
# Required file(s):
#   - Standardized beacon CSV file. 
#
# Notes:
#   - There is an added line to remove rows with NAs when calculating azimuth. 
#   - Is there another way to fix this?
#------------------------------------------------------------------------------

pre_polar_plot <- function(standard_data) {
  
  # Debug
  message("Executing script: pre_polar_plot.R")
  
  # Finding independent number of days 
  doy = as.numeric(strftime(standard_data$datetime_data, "%j"))
  differentDate = doy[1:length(doy) - 1]-doy[2:length(doy)]
  index = which(differentDate != 0) + 1  
  
  # Copy data and subset on index (not a 24 hr sampling but the day changed)
  dayDateTime = standard_data$datetime_data[index]
  dayLat = standard_data$latitude[index]
  dayLon = standard_data$longitude[index]
  daydt = as.numeric(difftime(dayDateTime[2:length(dayDateTime)],
                              dayDateTime[1:length(dayDateTime) - 1], units = "hours"))
  
  # Get the distance travelled between positions 
  dist = vector(mode = 'numeric', length = 0)
  az = vector(mode = 'numeric', length = 0)
  err = vector(mode = 'numeric', length = 0)
  
  for (i in 1:(length(dayLat) - 1) ) {
    vect = distaz(dayLat[i], dayLon[i],dayLat[i + 1], dayLon[i + 1])      
    dist = append(dist, vect$dist)
    az = append(az, vect$az)
    err = append(err, vect$err)
  }    # end distace function
  
  DayDist = data.frame(dist, az, err, daydt, dayDateTime[1:i])
  
  # Get rid of rows with NAs
  DayDist = DayDist[complete.cases(DayDist),]
  
  # Calculate the speed
  daySpeed = DayDist$dist / DayDist$daydt    # kph
  daySpeed = daySpeed*24    # km per day
  dayaz = DayDist$az
  
  # Get rid of the negatives
  for (i in 1:length(dayaz)) {
    if (dayaz[i] < 0) {dayaz[i] = 360+dayaz[i]}
  }    # End azimuth function
  
  dayaz = dayaz*pi/180
  
  # Plot
  # Source help: https://stat.ethz.ch/pipermail/r-help/2002-October/025964.html     
  polar_plot(daySpeed, dayaz, theta.zero = pi/2, theta.clw = TRUE, method = 1,
            rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
            grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
            lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
            polygon.bottom = TRUE, overlay = NULL, pi2.lab = FALSE, text.lab = c("N","E","S","W"),
           num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
            rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
            tlabel.col = "black", main = "Ice island daily velocity (km/d)", sub = filename)
  
  # Delete existing files
  delete_file(filename, c("pdf"))
  delete_file(paste("polarplot_", filename, sep = ""), c("png"))
  
  # Save polarplot to output file  
  dev.copy(png, paste("polarplot_", filename, ".png", sep = ""))
  dev.off()

} # End function