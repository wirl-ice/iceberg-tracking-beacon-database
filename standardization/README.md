# Iceberg Tracking Beacon Data Standardization

Author: Adam Garbo

Contributors: Anna Crawford, Jill Rajewicz and Derek Mueller, Carleton University

Date: 2020-04-11

## Introduction

This collection of code ingests raw tracking beacon data in comma-separated values (CSV) format, peforms a number of data cleaning steps and outputs a standardized CSV file.

## Description:

The R programming language was used to write a number of scripts to perform various data ingestion and cleaning tasks. 


The directory structure of the Iceberg Tracking Beacon Database is as follows:

### Directory structure:

```
.
├── analysis
│   ├── R				# R code for performing spatial analyses
│   └── Shapefiles			# Shapefiles used in spatial analyses
├── data				
│   └── <year>				# Year of collected tracking beacon data
│       └── <IMEI>			# Tracking beacon IMEI or unique identifier
│           ├── documentation		# Related project documentation
│           ├── photos			# Photos of tracking beacon deployments
│           ├── raw_data		# Raw tracking beacon data
│           │   ├── deployment_file	# Data prepared for ingestion to standardizations scripts
│           │   └── original_file	# Original, unmodified tracking beacon data
│           └── standardized_data	# Processed tracking beacon data CSV file
├── documentation			
│   └── manuals				# Available tracking beacon instrumentation manuals
├── output_data
│   ├── shapefiles			# Shapefiles (line and points) of tracking beacon trajectories
│   └── database			# Database snapshots in CSV format 
└── scripts
    ├── download_decode			# Python code to download and process SBD data
    ├── python				# Python scripts to merge standardized CSV files
    └── standardization			# R scripts to process raw tracking beacon data
    
```



## Usage:

### Installation:

The necessary R packages are listed on line 40 of [beacon_processing.R](
https://github.com/adamgarbo/CIS_Iceberg_Tracking_Beacon_Database/blob/367f82ced80b76886deb97f40e08c529fc0ea186/standardization/beacon_processing.R#L40)

At the Terminal command line interface, navigate to the `./scripts/standardization` directory. Ensure that parent file, beacon_processing.R, resides within this directory. 

beacon_processing.R will call other scripts as needed. A description of these scripts can be found below.

#### Syntax:
```R
Rscript beacon_processing.R <input_path> <output_path> <script_path> <filename> <beacon_type>
```

#### Arguments:
* input_path: 
  * Full path to input data folder (e.g. /iceberg_tracking_beacon_database/data/2018/300434063415160/raw_data/deployment_file)
* output_path      
  * Output data folder for processed data (e.g. /iceberg_tracking_beacon_database/data/2018/300434063415160/standardized_data )
* script_path          
  * Folder containing standardization scripts (e.g., /iceberg_tracking_beacon_database/scripts/standardization)
* filename        
  * Name of raw beacon data CSV file followed by the start year of the deployment, separated by an underscore, without extension (i.e. '12995_1997', *NOT* '12995_1997.csv')
* beacon_type 
  *  Currently supported beacon types include: BIO, CALIB_ARGOS,CALIB_IRIDIUM, CANATEC, CCGS, CRYOLOGGER, GNSS, IABP, NAVIDATUM, OCEANETIC, PPP, ROCKSTAR, SOLARA, SVP-I-BXGSA-L-AD, SVP-I-BXGS-LP, SVP-I-XXGS-LP
	  
#### Syntax example:

```R
Rscript beacon_processing.R /iceberg_tracking_beacon_database/data/2018/300434063415160/raw_data/deployment_file /iceberg_tracking_beacon_database/data/2018/300434063415160/standardized_data /iceberg_tracking_beacon_database/scripts/standardization 300434063415160_2018 CRYOLOGGER
```

## Outputs:

**1. Standardized CSV file**

A CSV will be produced with the following column headings:
```
beacon_id
beacon_type
datetime_data
datetime_transmit
latitude
longitude
vbat
ta
ti
ts
bp
pitch
roll
heading
satellites
loc_accuarcy
message_index
gps_delay
snr
ttff
```
**2. Geospatial outputs:**
* .shp/.shx/.prj/.dbf files (line and point)
* .gpx files (line and point)
* .kml files (line and point) 

**3. Graphic plots**
* A PDF file containing: PolarPlot (.png), SpeedPlot (.png) and cummulative speed probability plot (.png), 

**4. Statistics**
* A text ice island statistics file.

**5. Debugging log**
* A text file that includes information debugging information that is produced when each R script is called. Can be used to troubleshoot issues with standardizing a particular dataset or beacon type.

## Troubleshooting:
1. Some of the 'beacon types' are actually data delivery types (e.g. sbd) so need to make sure beacon type is manually entered
2. Canatec beacons are inconsistent - some have hh:mm:ss while some have hh:mm in the timestamp. Need to go into script and just comment out correct line to run it, depending on timestamp format
3. If the error below is written to the debug log file, navigate to the `../standardized_data` directory and remove the .kml files that were created. They are unable to be overwritten. 
```
"Error in writeOGR(Dataset, paste(getwd(), paste(filename, "_pt", ".kml",  : layer exists, use a new layer name
Calls: csv2kml -> writeOGR
Execution halted"
```
4. Possible errors: Assertion statement on line 167 (Validation function). If comes back false in command line window it means that there is an error with the standardized csv column data types.

### Error handling and logging
Errors are logged to the the debug.txt file in the `../standardized_data` output directory. It is appendable and messages will be compiled with every run. A timestamp is logged with each run WHERE AN ERROR OCCURS. Logging will occur even if no errors or warnings are initiated.  

#### Errors that will halt to program include:
1. An invalid number of command line arguments
2. An invalid `beacon_type` argument
3. If an error is encountered while reading in the beacon data
4. The standardized CSV columns have incorrect data types
5. If the program attempts to overwrite .kml files in the output directry 
6. Rplots.pdf not being written correctly

#### Errors that will log but allow the program to continue include:
1. The datetime columns of the standardized CSV being the incorrect format
2. The datetime columns of the standardized CSV not being in chronological order

## Contributing to the Iceberg Tracking Beacon Database
### Adding support for new beacon types: 

1. Create a new function script to standardize the raw CSV data to the standardized format (e.g. cryologger_to_csv, svp_to_csv)
2. Add this function to the sourced functions in beacon_processing.R (line 111-127)
3. Add this function to script `standardize_data.R`, along with the appropriate `else if` statement:
```R
else if (beacon_type == "CRYOLOGGER") {
    cryologger_to_csv(raw_data)
```
4. Add the beacon_type to the list of valid command line options at line 190-207: 
```R
# Check if beacon_type argument is valid. Add additional beacon_type as required.
valid = c("BIO",
          "CALIB_ARGOS",
          "CALIB_IRIDIUM",
          "CANATEC",
          "CCGS",
          "CRYOLOGGER",
          "GNSS",
          "IABP",
          "NAVIDATUM",
          "OCEANETIC",
          "PPP",
          "ROCKSTAR",
          "SOLARA",
          "SVP-I-BXGSA-L-AD",
          "SVP-I-BXGS-LP",
          "SVP-I-XXGS-LP",
          "WIRL")
```

### Changelog:
April 27, 2018
* New beacon types added
* New fields added to standardized CSV
* Additional cleaning step added to remove records with repeated time stamps
* Removed records exceeding beacon sensor ranges

June 15, 2019
* Complete overhaul of all scripts to move to new standardized column names
* Reworked scripts to create shape files so that full IMEI unique identifiers are used

April 11, 2020
* Documentation re-written to reflect significant changes to the overall project

## Publications
* To follow.
