# Iceberg Tracking Beacon Database
![Image](https://github.com/adamgarbo/cryologger-ice-tracking-beacon/blob/main/Archive/v2.0/Images/2019_300434063392070.JPG)

## Introduction
The Canadian Ice Service, in cooperation with Carleton University, have recently compiled one of the most comprehensive databases of in-situ iceberg tracking beacon drift trajectories in the Northern Hemisphere, with over 800,000 observations collected between 1997 and 2023. 

The database contains iceberg and ice island position and meteorological data as measured by tracking beacons as well as information about the target at the time of deployment (e.g. shape, dimensions, source, thickness) where possible. Data have been contributed by government, academic, and industry sources from tracking beacons deployed on targets in the western and eastern Canadian Arctic, and off the east coast of Canada. 

Drift direction, speed, and pattern data from satellite tracking beacons deployed on icebergs and ice islands will be used to understand how icebergs drift, and to develop and validate models of iceberg drift, in order to improve predictions of ice hazard occurrence and behaviour. 


## Repository Contents


MOVE TO README FOR DATABASE

```
.
|── documentation           # Database documentation
├── database                # 
|   |── metadata            # Metadata table in CSV format
│   ├── track_data			# All beacon tracks in CSV format
│   └── postgis	    		# or gpkg ?  a complete database
├── raw_data                # Raw tracking beacon data
│   └── <year>				# Year of collected tracking beacon data
│       └── <IMEI>			# Tracking beacon IMEI or unique identifier
│           ├── raw_data		# Raw tracking beacon data
│           │   ├── deployment_file	# Data prepared for ingestion to standardizations scripts
│           │   └── original_file	# Original, unmodified tracking beacon data
│           └── standardized_data	# Processed tracking beacon data CSV file
├── standardized_data	    # Processed beacon track data in CSV, GPKG and KML formats
│   └── <year>				# Year of collected tracking beacon data
│       └── <IMEI>			# Tracking beacon IMEI or unique identifier
├── deployment_notes	    # Ancillary information regarding the icebergs and deployments
│   └── <year>				# Year of collected tracking beacon data
│       └── <IMEI>			# Tracking beacon IMEI or unique identifier
├── photos	                # Photos and videos of the icebergs and deployments
│   └── <year>				# Year of collected tracking beacon data
│       └── <IMEI>			# Tracking beacon IMEI or unique identifier
├── dbeacon_model_info  	# Available tracking beacon instrumentation manuals, spec sheets and other info		
│   └── <Model>             # Organized by beacon model

Not included? 

└── scripts
    ├── download_decode			# Python code to download and process SBD data
    ├── python				# Python scripts to merge standardized CSV files
    └── standardization			# R scripts to process raw tracking beacon data

├── analysis
│   ├── figures                         # Spatial analysis produced figures
│   ├── Python				# Python code for plotting spatial data
│   ├── R		                # R code for performing spatial analyses
│   └── Shapefiles			# Shapefiles used in spatial analyses


```

## Publications
* To follow.

## License
* To follow.
