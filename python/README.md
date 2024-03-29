# Iceberg Beacon Database: Standardization
Author: Adam Garbo  
Date: 2022-12-16

## Introduction
The Python code ingests raw tracking beacon data in comma-separated values (CSV) format, peforms a number of data cleaning steps and outputs a standardized CSV file.

## Directory structure
The directory structure of the Iceberg Beacon Database is as follows:
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
│   ├── csv				# Shapefiles (line and points) of tracking beacon trajectories
│   └── shapefiles			# Database files in CSV format 
└── scripts
    ├── download_decode			# Python code to download and process SBD data
    ├── python				# Python scripts to merge standardized CSV files
    └── standardization			# R scripts to process raw tracking beacon data
```

## Python Code
`beacon_processing.py`
* Main Python script that contains functions to process raw beacon trajectory data
  * Recursively searches through all database folders to identify files to process
  * Selects appropriate conversion function
  * Standardizes data columns
  * Cleans data according to minimum/maximum values
  * Calculates velocity
  * Creates output files

`processing_functions.py`
* Contains Python functions to convert specific beacon types

`beacon_visualization.py` 
* Code to visualize beacon tracjectories and data

`rename_folders.py`
* Can be used to recursively search through the entire database and rename folders as desired

## Usage

## Outputs:

**1. Standardized CSV file**

A CSV will be produced with the following column headings:

| Variable | Unit |
| --- | ---  |
| beacon_id |   |
| beacon_type |   |
| datetime_data | YYYY-MM-DD hh:mm:ss |
| datetime_transmit | YYYY-MM-DD hh:mm:ss  |
| latitude | DD  |
| longitude | DD |
| temperature_air | °C  |
| temperature_internal | °C  |
| temperature_surface | °C |
| pressure | hPa |
| pitch | ° |
| roll | ° |
| heading | ° |
| satellites | # |
| voltage | V  |
| loc_accuarcy |   |
| distance | m |
| speed | m/s |

**2. Geospatial outputs:**
* Point shapefile (.shp/.shx/.prj/.dbf)

**3. Graphic plots**
* To follow 

**4. Statistics**
* To follow 

**5. Debug log**
* A text file that includes information debugging information that is produced when each function is called. Can be used to troubleshoot issues with standardizing a particular dataset or beacon type.


## Contributing to the Iceberg Tracking Beacon Database
### Adding support for new beacon types: 

1. Create a new function in `processing_functions` that will convert the raw CSV data to the standardized format (e.g. process_cryologger, svp_to_csv)
2. Add the appropriate IMEI and related function to `get_function()` dictionary lookup (e.g., `"2021_300434063291950": process_cryologger,`)

## Changelog:
2022-12-10
* Converted all iceberg beacon database processing scripts from the R programming language to Python

