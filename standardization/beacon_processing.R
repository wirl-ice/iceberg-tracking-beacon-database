#------------------------------------------------------------------------------
# Title: Compilation and standardization of iceberg tracking beacon data
#
# Author: Adam Garbo, Carleton University
#
# Date: April 10, 2020
#
# Contributions by: 
#   Jill Rajewicz, April 26, 2018
#   Anna Crawford, 2014 
#
# Changelog:
#   June 18, 2019
#     - Massive code overhaul to variables, standardized fields, comments, 
#     debugging output, examples, and more.
#     - Changed "Drift" variable to "raw_data"
#     - Changed "Beacon" variable to "standard_data"
#
# Description: 
#   - Creates a  standardized CSV file with individual functions for each beacon 
#   type/source
#   - Injests raw data downloaded from appropriate source  
#   - Calls on individual functions for various other output file types
#
# Necessary file(s): 
#   - Beacon data downloaded and converted to CSV format
#
# Command line directions: 
#   input directory, output directory, program directory, filename, beacon type
#   Example:
#   
# Notes: 
#   - Raw data should be kept in an "input" directory with other raw data
#   - Write command line without commas
#   - PolarPlot functions found through the heR.Misc library
#
#------------------------------------------------------------------------------------------------

# Install packages
p <- c("anytime", "assertthat", "chron", "date", "dplyr", "geosphere", "here", 
       "lubridate", "sp", "rgdal", "RPMG", "RSEIS", "shapefiles", "stringr")
#install.packages(p) # Warning: Un-commenting this may take several minutes

# Load the required packages
lapply(p, library, character.only = TRUE)

# Disable scientific notation
options(scipen = 999)

# Define system arguments
#-------------------------
args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
script_dir <- args[3]
filename <- args[4]
beacon_type <- args[5]

# Example variables for debugging
#--------------------------------
# ARGOS
#beacon_type <- "CALIB_ARGOS"
#filename <- "12995_1997"
#input = "~/Desktop/iceberg_tracking_beacon_database/data/1997/12995/raw_data/deployment"
#output = "~/Desktop/iceberg_tracking_beacon_database/data/1997/12995/standardized_data"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"

# BIO
#beacon_type <- "BIO"
#filename <- "300034012519990"
#input = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization/test"
#output = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization/test"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"

# # MetOcean CALIB 
# beacon_type <- "SBD"
# filename <- "751700"
# input = "~/Desktop/CIS/BeaconData/csv/"
# output = "/home/dmueller/Desktop/CIS/BeaconData/std/"
# script_dir = "~/Documents/GitHub/CIS_Iceberg_Beacon_Database/standardization"

# Cryologger #2
#beacon_type <- "CRYOLOGGER"
#filename <- "300434063415110"
#input = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization/test"
#output = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization/test"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"

# Canatec
#beacon_type <- "CANATEC"
#filename = "26973_2009"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"
#input = "~/Desktop/iceberg_tracking_beacon_database/data/2009/26973/raw_data/deployment"
#output =  "~/Desktop/iceberg_tracking_beacon_database/data/2009/26973/"

# CCGS
#beacon_type <- "CCGS"
#filename = "300034013149880_2011"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"
#input = "~/Desktop/iceberg_tracking_beacon_database/data/2011/300034013149880/raw_data/deployment"
#output =  "~/Desktop/iceberg_tracking_beacon_database/data/2011/300034013149880/"

# Oceanetic
#beacon_type <- "OCEANETIC"
#filename = "300034013463170_2011"
#script_dir = "~/Desktop/iceberg_tracking_beacon_database/scripts/standardization"
#input = "~/Desktop/iceberg_tracking_beacon_database/data/2011/300034013463170/raw_data/deployment"
#output =  "~/Desktop/iceberg_tracking_beacon_database/data/2011/300034013463170/"
  
# Source scripts
#----------------
setwd(script_dir)

# Functions to convert raw data to standardized CSV
source("bio_to_csv.R")
source("calib_argos_to_csv.R")
source("calib_iridium_to_csv.R")
source("canatec_to_csv.R")
source("ccgs_to_csv.R")
source("cryologger_to_csv.R")
source("distaz.R")
source("gnss_to_csv.R")
source("iabp_to_csv.R")
source("iip_to_csv.R")
source("navidatum_to_csv.R")
source("oceanetic_to_csv.R")
source("rockstar_to_csv.R")
source("sbd_to_csv.R")
source("solara_to_csv.R")
source("svp_to_csv.R")
source("standardize_data.R")

# Function to clean raw data
source("clean_data.R")
source("standardize_columns.R")
source("sensor_range.R")

# Assertion function to check standardization procedure
source("validate.R")

# Function to calculate speed and distance
source("calculate_speed.R")

# Functions to convert standardized CSV to quality added file types
source("csv_to_gpx.R")
source("csv_to_kml.R")
source("csv_to_shp.R")

# Functions to convert standardized CSV to R plots
source("cumulative_speed.R") # TO UPDATE
source("polar_plot.R")
source("polar_plot_type.R")
source("pre_polar_plot.R")
source("pre_polar_plot_ppp.R")
source("speed_plot.R")

# Function to output a summary stats file
source("summary_stats.R")

# Function to overwrite all files (Backup first if you want to save the files!)
source("delete_file.r")

# Capture messages and errors to log file in output directory
#-----------------------------------------------------------
setwd(output)

# Delete all pre-existing files within output folder
unlink("*")

# Create logfile 
logfile <- file("debug.txt", open="wt")

# Divert R console message stream to logfile (cannot be split)
sink(logfile, type="message", append=TRUE, split = FALSE) 

# Print system time and arguments to logfile
message("Datetime: ", Sys.time())
message("Beacon type: ", beacon_type)
message("Filename: ", filename)
message("Script directory: ", script_dir)
message("Input directory: ", input)
message("Output directory: ", output)

# Check if sink is diverting streams
#sink.number(type="message")
#sink.number(type="output")

# Validate input data paramters
#------------------------------
# Check if correct number of arguments included at command line 
if(length(args) != 5) { 
  stop("Incorrect number of input arguments: ", length(args))
} else {
  message("Check: Correct number of input arguments")
} # End check

# Check if beacon_type argument is valid. Add additional beacon_type as required.
valid = c("BIO",
          "CALIB_ARGOS",
          "CALIB_IRIDIUM",
          "CANATEC",
          "CCGS",
          "CRYOLOGGER",
          "GNSS",
          "IABP",
          "IIP",
          "NAVIDATUM",
          "OCEANETIC",
          "PPP",
          "ROCKSTAR",
          "SOLARA",
          "SVP-I-BXGSA-L-AD",
          "SVP-I-BXGS-LP",
          "SVP-I-XXGS-LP",
          "WIRL",
          "SBD")

if(!(beacon_type %in% valid)) {
  stop("'beacon_type' invalid")
} else {
  message("Check: 'beacon_type' valid")
} # End check

# Read raw data from CSV file
#------------------------------
setwd(input)
raw_data <- read.csv(paste(filename, ".csv", sep=""), header = T, sep = ",", dec = ".", na.strings = c("NA", "NULL"), strip.white = FALSE)

# Check if beacon data is read properly
if (class(raw_data) != "data.frame") {
  stop("Beacon data not read correctly") 
} else {
  message("Check: Beacon data read correctly")
} # End check

# Process raw data
#------------------
# Executes function linked to beacon source/type. Converts raw data to standardized CSV format.
setwd(output)
standardize_data(beacon_type, raw_data)

# Convert standardized CSV files to quality added files 
#-------------------------------------------------------
# Read in CSV data located in output directory. This calls the QC data file which has had duplicate.
# The following files will also be written to output directory.
setwd(output)
standard_data <- read.csv(paste(filename, ".csv", sep = ""), header = TRUE , 
                          sep = ",", dec = ".", na.strings = c("NA", "NULL"), 
                          strip.white = FALSE, stringsAsFactors = FALSE)

# Check if standardized data has correct format. Returns "True" or "False"
validation(standard_data)

# Check if time is in correct format. Outputs a warning to log file but will continue to run.
date <- try(as.Date(standard_data[2:length(standard_data$datetime_data), 3], format = "%Y-%m-%d %H:%M:%S")) 

if (class( date ) == "try-error" || is.na ( date )) {
  warning ("'datetime_data' does not have correct format") # Remove seconds
} else {
  message("Check: 'datetime_data' has correct format")
}

# Check if data is ordered chronologically
# Outputs a warning to log file but will continue to run.
# Get the time difference between positions & make sure all are positive
chronDate <- as.numeric(difftime(standard_data$datetime_data[2:length(standard_data$datetime_data)],
                                 standard_data$datetime_data[1:length(standard_data$datetime_data)-1], units = "hours")) 

if (any(chronDate < 0) == "TRUE") {
  warning("Standardized CSV is not ordered chronologically ") 
} else {
  message("Check: Standardized CSV is ordered chronologically")
} # End check

# Convert from standardized CSV to point shapefile.
#---------------------------------------------------
# Note: If missing seconds from .dbf/.shp file, format CSV gps_time cell to HH:MM:SS
setwd(output)
csv_to_shp(standard_data, filename) 

# Note if using Arc:
#   Before importing to ArcInfo make a text file called *.prj with the following line:
#   That will tell ArcInfo that it is not projected - WGS84 coords.
#   copy the file:  WGS1984.prj, which contains the following:
#   GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],
#   PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]
#file.copy("WGS1984.prj", paste(buoy, ".prj", sep=""), overwrite = TRUE)

# Convert from standardized CSV to KML
#--------------------------------------
# Note: .kml files cannot be overwritten. 
# Move these to a different directory if file name exists in output directory.
# This error will cause the program to halt and will be reported in the logfile.
csv_to_kml(standard_data, filename)

# Convert from standardized CSV to GPX
#--------------------------------------
csv_to_gpx(standard_data, filename)

# Convert CSV to polar plot (polarplot called within prepolarplot function)
#-----------------------------------------------------------------------------
#polar_plot_type(standard_data)
#pre_polar_plot(standard_data) 

# Convert CSV to speed plot
#-------------------------
#speed_plot(standard_data)

# Convert CSV to cumulative speed plot
#-------------------------------------
#cumulative_speed(standard_data)

# Produce text file of summary stats
#-----------------------------------
setwd(output)
summary_stats(standard_data)

# Confirm generation of plots
#----------------------------
# Check that Rplots.pdf (Contains polarplot, speed plot and cumulative_speed plot) were created and saved.
if (!file_test("-f", paste("Rplots", ".pdf",sep=""))) 
{
  stop("Plot PDF not written")
} else {
  message("Check: PDF plot written")
} # End check

# Halt message and error logging
#--------------------------------
sink(type="message")

# Show debug log
#file.show("debug.txt")

