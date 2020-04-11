# Introduction

Author: Adam Garbo

Date: 2020-04-11

## Iceberg Tracking Beacon Database: Standardization Code

This collection of code ingests raw tracking beacon data in comma-separated values (CSV) format and outputs a standardized CSV file, does some basic cleaning, and outputs other quality added

file types - point and line kml files, point and line shape files, and gpx files

beacon_processing.R: is the parent file, and calls on various other files (described below).

**Directory structure:**

raw_data
deployment_file
original_file
standardized_data

At the command line, navigate to the `/standardization` directory. 
Keep the driver script (beacon_processing.R) in this main directory

###Description:

**Syntax:**

`Rscript beacon_processing.R <input_path> <output_path> <script_path> <filename> <beacon_type>`

**Arguments:**
* input_path 
  * Path to input data folder full path(e.g. /iceberg_tracking_beacon_database/data/2018/300434063415160/raw_data/deployment_file)
* output_path      
  * Output data folder for processed data (e.g. /iceberg_tracking_beacon_database/data/2018/300434063415160/standardized_data )
* script_path          
  * Folder containing scripts (except driver script) (e.g., /iceberg_tracking_beacon_database/scripts/standardization)
* filename        
  * Name of raw beacon data CSV file followed by the start year of the deployment, separated by an underscore, without extension (i.e. '12995_1997', *NOT* '12995_1997.csv')
* beaconType 
  *  Currently supported beacon types: BIO, CALIB_ARGOS,CALIB_IRIDIUM, CANATEC, CCGS, CRYOLOGGER, GNSS, IABP, NAVIDATUM, OCEANETIC, PPP, ROCKSTAR, SOLARA, SVP-I-BXGSA-L-AD, SVP-I-BXGS-LP, SVP-I-XXGS-LP
	  
**Syntax example:**

`Rscript beacon_processing.R /iceberg_tracking_beacon_database/data/2018/300434063415160/raw_data/deployment_file /iceberg_tracking_beacon_database/data/2018/300434063415160/standardized_data /iceberg_tracking_beacon_database/scripts/standardization 300434063415160_2018 CRYOLOGGER`

**Expected output:**

1) A standardized CSV file with the following column headings: 

* beacon_id
* beacon_type
* datetime_data
* datetime_transmit
* latitude
* longitude
* vbat
* ta
* ti
* ts
* bp
* pitch
* roll
* heading
* satellites
* loc_accuarcy
* message_index
* gps_delay
* snr
* ttff
  
2. shp/shx/prj/dbf files (line and point), gpx files (line and point), kml files (line and point), 

3. A pdf with: PolarPlot (png), SpeedPlot (png) and cummulative speed probability plot (png), 

4. A text ice island statistics file

**Troubleshooting:**
1. Some of the 'beacon types' are actually data delivery types (e.g. sbd) so need to make sure beacon type is manually entered
2. Canatec beacons are inconsistent - some have hh:mm:ss while some have hh:mm in the timestamp. Need to go into script and just comment out correct line to run it, depending on timestamp format
3. If this statement is written to the log: "Error in writeOGR(Dataset, paste(getwd(), paste(filename, "_pt", ".kml",  : 
  layer exists, use a new layer name
Calls: csv2kml -> writeOGR
Execution halted"
	Go into your ProcessedBeaconData directory and remove kml files already written for that beacon. kml files cannot be overwritten. 
4. Possible errors: Assertion statement on line 167 (Validation function). If comes back false in command line window it means that there is an error with the standardized csv column data types.
APRIL 2018: This function is not working. Commented out for now.
5. Necessary packages are listed between Lines 31-44 of BeaconProcessing.R. Please load these if they are not already on your system. 

To add another beacon type, you must: 

1. Create a new function script to standardize the raw csv data to the standardized format (see Canatec2csv, ArgosCALIB2csv, etc. for 
templates)
2. Add this function to the sourced functions in BeaconProcessing.R (line 58)
3. Add this function to script 'raw2csv.R', along with the appropriate 'if' statement (e.g., if (beaconType == "Canatec") {
      Canatec2csv(Drift))
4. Add the beaconType to the list of valid command line options at line 125: valid = c("ArgosCALIB", "DFO", "Oceanetics", "Canatec")
5. Add the beaconType to the list of available options at the command line (beginning of this document). 

**Error handling and logging**
Errors are logged to the output directory in BeaconProcessingLog.txt. It is appendable and messages will be compiled with every run. A timestamp is logged with each run WHERE AN ERROR OCCURS. No logging will occur if there is no errors or warnings are initiated.  

**Errors that will halt to program include:**
1. An incorrect number of command line arguments
2. beacon_type argument is incorrect (not a possible option)
3. If there is an error reading in the beacon data
4. The standardized csv columns having incorrect data types
5. Attempting to force the program to overwrite kml files in the output directry 
6. Rplots.pdf not being written

**Errors that will log but allow the program to continue include:**

1. The datetime columns of the standardized CSV being the incorrect format
2. The datetime columns of the standardized CSV not being in chronological order


**beacon_cleaning:**

This is a start at a script to further clean and quality control beacon data.

The aim of this script is to diagnose when a beacon file should be terminated
( e.g. fell off target into water, or picked up by ship)

#input: beacon file with standardized headings (beginning may be trimmed according to deployment notes/observations etc)

Script should:

#1) calculate a running standard deviation of temperature (where applicable)
#plot temp/std dev/running mean  of temp
#2) calculate distance travelled and speed - then identify points where there are big changes in these things - impossible speeds
 or jumps in speed or decline in temp variation

#3) smooth beacon tracks using a Kalman filter

#4) fill data gaps? if necessary


June 15, 2019
- Massive overhaul of all scripts to move to new standardized column names
- Reworked scripts to create shape files so that full IMEI unique identifiers are used

Compiled by Anna Crawford, PhD Student, DGES, Carleton Univeristy
Previously written scripts provided by Derek Mueller, Assistant Professor, DGES, Carleton University
Modified by Jill Rajewicz in 2017-2018 (new beacon types added, new fields added to standardized csv, additional cleaning step added to get rid of records with repeated time stamps, records
removed which exceed beacon sensor ranges).

Compiled in February 2014
Last modified by: Anna Crawford, April 16, 2014
Last modified by: Jill Rajewicz, April, 27 2018
Last modified by: Adam Garbo, June 15, 2019










