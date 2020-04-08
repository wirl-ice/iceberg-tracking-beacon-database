#--------------------------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Derek Mueller, Carleton University, 2011
#
# Modified by: 
#   Anna Crawford, June 24, 2015
#   Adam Garbo, March 21, 2019
#
# Project: Ice island drift, deterioration and detection 
#
# Description: 
#   Function to convert standardized beacon csv to polarplot with a minute unit of length
#   Called on by BeaconProcessing.R and PrePolarPlot.R
#
# Required file(s):
#   Processed/standardized beaconcsv file
#
# Notes:
#   There is an added line to remove rows with NAs when calculating azimuth. 
#   Is there another way to fix this?
#--------------------------------------------------------------------------------------------------
pre_polar_plot_ppp <- function(Beacon) {
  
  # Debug
  message("***Script: pre_polar_plot_ppp***")
  
  # Finding independent number of minutes 
  mod = as.numeric(substr(strftime(Beacon$gps_time),15,16)) #mod = minute of day
  differentMin = mod[1:length(mod) - 1]-mod[2:length(mod)]
  index = which(differentMin != 0) + 1  
  
  # Copy data and subset on index (on the minute changed)
  minDateTime = Beacon$gps_time[index]
  minLat = Beacon$lat[index]
  minLon = Beacon$lon[index]
  mindt = as.numeric(difftime(minDateTime[2:length(minDateTime)],
                              minDateTime[1:length(minDateTime) - 1], units = "mins"))
  
  # Get the distance travelled between positions 
  dist = vector(mode = 'numeric', length = 0)
  az = vector(mode = 'numeric', length = 0)
  err = vector(mode = 'numeric', length = 0)
  
  for (i in 1:(length(minLat) - 1) ) {
    vect = Distaz(minLat[i], minLon[i],minLat[i + 1], minLon[i + 1])      
    dist = append(dist, vect$dist)
    az = append(az, vect$az)
    err = append(err, vect$err)
  }    # end distace function
  
  minDist = data.frame(dist, az, err, mindt, minDateTime[1:i])
  
  # Get rid of rows with NAs
  minDist = minDist[complete.cases(minDist),]
  
  # Calculate the speed
  minSpeed = minDist$dist / minDist$mindt    # kpminute
  minSpeed = minSpeed*60    # km per hour
  minaz = minDist$az
  
  # Get rid of the negatives
  for (i in 1:length(minaz)) {
    if (minaz[i] < 0) {minaz[i] = 360+minaz[i]}
  }    # End azimuth function
  
  minaz = minaz*pi/180
  
  # Plot
  # Source help: https://stat.ethz.ch/pipermail/r-help/2002-October/025964.html     
  polar_plot(minSpeed, minaz, theta.zero = pi/2, theta.clw = TRUE, method = 1,
            rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
            grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
            lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
            polygon.bottom = TRUE, overlay = NULL, pi2.lab = FALSE, text.lab = c("N","E","S","W"),
            num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
            rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
            tlabel.col = "black", main = "Ice island daily velocity (km/min)", sub = fname1)
  
  # Delete existing files
  delete_file(paste("polarplot_", fname1, sep = ""), c("png"))
  
  # Save polarplot to output file  
  dev.copy(png, paste("polarplot_", fname1, ".png", sep = ""))
  dev.off()
} # End function
