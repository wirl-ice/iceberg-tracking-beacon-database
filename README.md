# Canadian Ice Service (CIS) Iceberg Beacon Database
![Image](https://github.com/adamgarbo/Cryologger_Iceberg_Tracking_Beacon/blob/master/Archive/V2/Images/DSC_2436.JPG)

## Introduction
The Canadian Ice Service has recently compiled one of the most comprehensive databases of in-situ iceberg tracking beacon drift trajectories in the Northern Hemisphere, with over 800,00 observations collected between 1997 and 2019. 

The database contains iceberg and ice island position and meteorological data as measured by tracking beacons as well as information about the target at the time of deployment (e.g. shape, dimensions, source, thickness) where possible. Data have been contributed by government, academic, and industry sources from tracking beacons deployed on targets in the western and eastern Canadian Arctic, and off the east coast of Canada. 

Drift direction, speed, and pattern data from satellite tracking beacons deployed on icebergs and ice islands will be used to understand how icebergs drift, and to develop and validate models of iceberg drift, in order to improve predictions of ice hazard occurrence and behaviour. 


## Directory structure:

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

## Publications
* To follow.

## License
* To follow.
