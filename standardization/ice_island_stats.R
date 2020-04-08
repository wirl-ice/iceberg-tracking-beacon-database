#------------------------------------------------------------------------------
# Title: Quality added output files
#
# Created by: Anna Crawford, Carleton University, 
#
# Date: February 28, 2014
#
# Modified by: 
#   Anna Crawdford, March 6, 2018
#   Adam Garbo, June 18, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert processed/standardized CSV beacon data to a 
#   - summarized text file.
#   - Called on by beacon_processing.R
#   
# Required file(s): 
#   - Standardized beacon CSV
#
# Notes: 
#   - Raw data should be kept in an 'input' directory with other raw data.
#------------------------------------------------------------------------------

ice_island_stats <- function(standard_data) {

  # Debug message
  message("Executing script: ice_island_stats.R")
  
  # Find day of year
  doy = as.numeric(strftime(standard_data$datetime_data, "%j"))
  
  # Determine number of differet days in beacon record
  dates_different = doy[1:length(doy) - 1] - doy[2:length(doy)]
  index = which(dates_different != 0) + 1  
  
  # Copy data and subset on index (not a 24 hr sampling but the day changed)
  day_datetime = standard_data$datetime_data[index]
  day_latitude = standard_data$latitude[index]
  day_longitude = standard_data$longitude[index]
  day_dt = as.numeric(difftime(day_datetime[2:length(day_datetime)],
                              day_datetime[1:length(day_datetime)-1], units="hours"))
  
  # Get the hourly distance travelled between positions 
  distance_hourly = vector(mode = 'numeric', length=0)
  for (i in 1:(length(standard_data$latitude) - 1)) {
    distance_hourly[i] = distGeo(c(standard_data$longitude[i], standard_data$latitude[i]), c(standard_data$longitude[i + 1], standard_data$latitude[i + 1]))
  }
  
    # Get the daily distance travelled between positions 
  distance = vector(mode = 'numeric', length=0)
  for (i in 1:(length(day_latitude) - 1)) {
    distance[i] = distGeo(c(day_longitude[i], day_latitude[i]), c(day_longitude[i + 1], day_latitude[i + 1]))
  }
  day_distance = data.frame(distance, day_dt, day_datetime[1:i])
  
  # Calculate the speed (distance traveled per day)
  speed_per_day = (day_distance$distance/1000) / day_distance$day_dt    # kph
  speed_per_day = speed_per_day * 24    # km per day

  # Statistics to put into the text file
  # ------------------------------------
  # Start, end and total days tracked
  standard_data$datetime_data <- ymd_hms(standard_data$datetime_data, tz='UTC')
  date_start <- min(standard_data$datetime_data)
  date_end <- max(standard_data$datetime_data)
  days_total = round((max(standard_data$datetime_data) - min(standard_data$datetime_data)))
  distance_total_daily = round((sum(distance/1000)),2)
  distance_total_hourly = round((sum(distance_hourly/1000)),2)
  
  # Average speed, temperature, pressure, battery voltage
  pressure_mean <- round((mean(standard_data$bp, na.rm = TRUE)),2) # Pa 
  voltage_mean <- round((mean(standard_data$vbat, na.rm = TRUE)),2) # V
  speed_mean <- round((mean(speed_per_day, na.rm = TRUE)),2) # kpd 
  ti_mean <- round((mean(standard_data$ti, na.rm = TRUE)),2) # °C  
  ts_mean <- round((mean(standard_data$ts, na.rm = TRUE)),2) # °C 
  ta_mean <- round((mean(standard_data$ta, na.rm = TRUE)),2) # °C 
  
  # Location & distance
  latitude_end <- tail(day_latitude, n=1)
  longitude_end <- tail(day_longitude,n=1)
  latitude_start <- day_latitude[1]
  longitude_start <- day_longitude[1]
        
  # Overwrite past summary files - backup first if you want to save the files!
  delete_file(paste("summary_", filename, sep = ""), c("txt"))
  
  # Write text file to output directory with above summary information
  summary <- as.character(paste("summary_", filename, ".txt", sep = ""))
     writeLines(c(paste("Summary information for beacon", filename, sep = " "),
                  paste("---------------------------------------------------"),
                  paste("Start Date:", date_start, sep = " "), 
                  paste("End Date:", date_end, sep = " "),
                  paste("Total Days Tracked:", days_total, sep =" "),
                  paste("Total Daily Distance Travelled (km):", distance_total_daily, sep =" "),
                  paste("Total Hourly Distance Travelled (km):", distance_total_hourly, sep =" "), " ",
                  paste("Statistics: Mean Values"),
                  paste("-----------------------"),
                  paste("Speed (kph):", speed_mean, sep = " "), 
                  paste("Temperature Internal (°C):", ti_mean, sep = " "), 
                  paste("Temperature Surface (°C):", ts_mean, sep = " "), 
                  paste("Temperature Air (°C):", ta_mean, sep = " "), 
                  paste("Battery Voltage (V):", voltage_mean, sep = " "), 
                  paste("Atmospheric Pressure (hPa):", pressure_mean, sep = " "), " ",
                  paste("Starting Latitude:", latitude_start, "Starting Longitude:", longitude_start, sep = " "), 
                  paste("Ending Latitude:", latitude_end, "Ending Longitude:", longitude_end, sep = " ")), summary)
  
  # Check that text file was written
  if (!file_test("-f", paste("summary_", filename,".txt",sep=''))) {
    stop("Summary statistics file unable to be created.")
  } else {
    message("Check: Summary statistics file created.")
  }
} # End function     
   