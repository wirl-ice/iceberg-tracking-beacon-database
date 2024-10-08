# Iceberg Tracking Beacon Database: Standardization, Visualization and Collation 

Authors: Adam Garbo, Derek Mueller 
with earlier contributions by Jill Rajewicz, Anna Crawford and Cindy Lopes 

Date: 2024-07-31

## Introduction
This Python code ingests raw tracking beacon data, standardizes it to a common format, peforms a number of data cleaning steps and outputs visualization and data products.

## Python Code
`track_processing.py`
* Main Python script that contains functions to process raw beacon trajectory data
  * Recursively searches through all database folders to identify files to process
  * Selects appropriate conversion function
  * Standardizes data columns
  * Cleans data according to minimum/maximum values
  * Calculates velocity
  * Creates output files

`standardization_functions.py`
* Contains Python functions to convert specific beacon types. This module is imported by `track_processing.py` and does not work on its own. 
* Includes the standardized format definition
* A template exists to add more functions as needed.  

`track_visualization.py` 
* Code to visualize beacon tracks and data. It reads a standardized csv and produces: 
  * a map of the iceberg track: `*_map.png`
  * a graph of variables over time - temperature, displacement and velocity: `*_time.png`
  * a graph of statistical distributions - polar plot of direction, histogram of speed: `*_dist.png`
  * a graph of temperature changes (used for checking if beacon is still on target): `*_temp.png`
* This script can be run at the command line and takes the following arguments: 
  * `std_file` : full path to the standard data file
  * `-p` : enter path to store output png files ; defaults to the current directory
  * `-g` : {time,map,temp,dist} list the graphs to produce; defaults to producing ALL graphs

`rename_folders.py`
* Can be used to recursively search through the entire database and rename folders as desired

## Inputs:

**1. Raw Data CSV file**

These are files containing the beacon raw data.  They *must* be named as follows: `yyyy_id.*` where yyyy is the year of deployment and id is the IMEI or other identifier

**2. Metadata CSV file**

This file contains key information on each track, including: 
* the appropriate standardization function to use
* the track start and end times
* the beacon model

**2. Beacon Spec CSV file**

This file contains the valid ranges of the data, used for cleaning

## Usage

## Outputs:

**1. Standardized CSV file**

A CSV will be produced with the following column headings:

| Variable | Unit |
| --- | ---  |
| beacon_id |   |
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
* Track point files (*_pt.*)
  * kml
  * gpkg
* Track line files (*_ln.*)
  * kml
  * gpkg

**3. Visualization plots:**
  * a map of the iceberg track: `*_map.png`
  * a graph of variables over time - temperature, displacement and velocity: `*_time.png`
  * a graph of statistical distributions - polar plot of direction, histogram of speed: `*_dist.png`
  * a graph of temperature changes (used for checking if beacon is still on target): `*_temp.png`

**4. Debug log**
* A text file that includes debugging information from `track_processing.py`. Can be used to troubleshoot issues with standardizing a particular dataset or beacon type.


## Contributing to the Iceberg Tracking Beacon Database
### Adding support for new beacon types: 
* Create new functions in `standardization_functions.py` that will convert the raw data to the standardized format using the template provided. 

## Changelog:
2022-12-10
* Converted all iceberg beacon database processing scripts from the R programming language to Python
2024-07-3
* Refactored code to 
