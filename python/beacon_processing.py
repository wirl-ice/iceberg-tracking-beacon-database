#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 10:59:44 2022

@title: Compilation and standardization of iceberg tracking beacon data
@author: Adam Garbo

"""

import os
import datetime as dt
from datetime import datetime
import logging
import glob
from pathlib import Path
import shutil
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, LineString, shape
import fiona 
import numpy as np
import pyproj
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
formatter = logging.Formatter(
    "%(asctime)s:%(msecs)d %(name)s %(levelname)s %(message)s", "%Y-%m-%d %H:%M:%S"
)

# Enable fiona driver
gpd.io.file.fiona.drvsupport.supported_drivers['KML'] = 'rw'

# -----------------------------------------------------------------------------
# Folder paths
# -----------------------------------------------------------------------------

# Specify input path (to do: make into an argument)
path_input = "/Volumes/data/iceberg_tracking_beacon_database/data"

# Local testing
path_input = "/Users/adam/Desktop/iceberg_beacon_database"


# -----------------------------------------------------------------------------
# Beacon deployment test files to confirm function operation
# -----------------------------------------------------------------------------

# BIO
file = path_input + "/data/2009/300034012616000/raw_data/deployment_file/2009_300034012616000.csv"

# Cryologger
file = path_input + "/data/2018/300434063415160/raw_data/deployment_file/2018_300434063415160.csv"

# CALIB ARGOS
file = path_input + "/data/2009/16795/raw_data/deployment_file/2009_16795.csv"

# CALIB Iridium
file = path_input + "/data/2014/300234061763040/raw_data/deployment_file/2014_300234061763040.csv"

# Canatec
file = path_input + "/data/2009/26973/raw_data/deployment_file/2009_26973.csv"

# CCG
file = path_input + "/data/2011/300034013458130/raw_data/deployment_file/2011_300034013458130.csv"

# IABP
file = path_input + "/data/2016/300234062950220/raw_data/deployment_file/2016_300234062950220.csv"

# IIP
file = path_input + "/data/2019/3037-2674613/raw_data/deployment_file/2019_3037-2674613.csv"

# Navidatum
file = path_input + "/data/2012/100000000000000/raw_data/deployment_file/2012_100000000000000.csv"

# Oceanetic
file = path_input + "/data/2011/300034013463170/raw_data/deployment_file/2011_300034013463170.csv"

# RockSTAR
file = path_input + "/data/2016/300234060172440/raw_data/deployment_file/2016_300234060172440.csv"

# Solara
file = path_input + "/data/2018/300234066241900/raw_data/deployment_file/2018_300234066241900.csv"

# SVP
file = path_input + "/data/2015/300234060104820/raw_data/deployment_file/2015_300234060104820.csv"

# Load test data
raw_data = pd.read_csv(file, index_col=False, skipinitialspace=True)

# Display columns
raw_data.columns

# -----------------------------------------------------------------------------
# Execute function(s)
# -----------------------------------------------------------------------------

# Process all data
process_data(path_input)


# Problem IDs:
2009_300034012571050
2010_300034012592660
   
# Generate database
create_database(path_input)

# Create figures
visualize_maps(path_input)
visualize_graphs(path_input)

# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------

def process_data(path_input):
    """

    Batch process iceberg tracking beacon deployment CSV files.

    Parameters
    ----------
    path_input : string
        Path to iceberg beacon database folder.

    Returns
    -------
    None.

    """

    # Recursively search for all files to be processed
    files = sorted(
        glob.glob(path_input + "/**/raw_data/deployment_file/*.csv", recursive=True)
    )
    
    # Start with most recent datasets first
    files.reverse()
    
    # Process all files
    for file in files:
        
        try:
            # Get standardized data output path
            path_output = Path(file).resolve().parents[2] / "standardized_data"
    
            # Delete existing files in output path
            files = glob.glob(str(path_output) + "/*")
            for f in files:
                try:
                    os.remove(f)
                except OSError:
                    pass
    
                      
            # Get unique beacon ID
            filename = Path(file).stem
    
            # Set log file path and name
            logfile = "{}/{}.log".format(path_output, filename)
    
            # Create file handler and set formatter
            file_handler = logging.FileHandler(logfile, mode="w")
            file_handler.setFormatter(formatter)
    
            # Add handler to the logger
            logger.addHandler(file_handler)
    
            logger.info("Processing {}".format(filename))
            print("Processing {}".format(file))
    
            # Load beacon deployment file CSV
            raw_data = pd.read_csv(file, index_col=False, skipinitialspace=True)
    
            # Select appropriate processing function
            function_to_call = get_function(filename)
    
            # Process beacon deployment file
            processed_data = function_to_call(file, raw_data)
    
            # Clean data
            cleaned_data = clean_data(processed_data)
    
            # Calculate speed and direction
            standardized_data = calculate_velocity(cleaned_data)
    
            # Create output files
            create_output_files(file, cleaned_data)
    
            # Close the log file
            file_handler.close()
    
            # Remove the handler from the logger. The default behavior is to pop out
            # the last added one, which is the file_handler added in the beginning of
            # this iteration.
            logger.handlers.pop()
            
            #break
        except OSError:
            pass

def clean_data(input_data):
    """
    Assigns NaN to sensor values that exceed the minimum/maximum ranges.

    Parameters
    ----------
    input_data : Pandas dataframe
        A Pandas dataframe with standardized columns.

    Returns
    -------
    A cleaned Pandas dataframe.

    """

    # Assign new name to dataframe
    df = input_data

    logger.info("Executing: clean_data()")

    # Latitude
    df.loc[
        (df["latitude"] >= latitude_max) | (df["latitude"] <= latitude_min), "latitude"
    ] = np.nan

    # Longitude
    df.loc[
        (df["longitude"] >= longitude_max)
        | (df["longitude"] <= longitude_min)
        | (df["longitude"] == 0),
        "longitude",
    ] = np.nan

    # Air temperature
    df.loc[(df["temperature_air"] >= temperature_air_max) | (df["temperature_air"] <= temperature_air_min), "temperature_air"] = np.nan

    # Internal temperature
    df.loc[(df["temperature_internal"] >= temperature_internal_max) | (df["temperature_internal"] <= temperature_internal_min), "temperature_internal"] = np.nan

    # Surface temperature
    df.loc[(df["temperature_surface"] >= temperature_surface_max) | (df["temperature_surface"] <= temperature_surface_min), "temperature_surface"] = np.nan

    # Pressure
    df.loc[(df["pressure"] >= pressure_max) | (df["pressure"] <= pressure_min), "pressure"] = np.nan

    # Pitch
    df.loc[(df["pitch"] >= pitch_max) | (df["pitch"] <= pitch_min), "pitch"] = np.nan

    # Roll
    df.loc[(df["roll"] >= roll_max) | (df["roll"] <= roll_min), "roll"] = np.nan

    # Heading
    df.loc[
        (df["heading"] >= heading_max) | (df["heading"] <= heading_min), "heading"
    ] = np.nan

    # Satellites
    df.loc[
        (df["satellites"] >= satellites_max) | (df["satellites"] <= satellites_min),
        "satellites",
    ] = np.nan

    # Battery voltage
    df.loc[(df["voltage"] >= voltage_max) | (df["voltage"] <= voltage_min), "voltage"] = np.nan

    # Drop all rows where latitude or longitude is nan
    df.dropna(subset=["latitude", "longitude"], inplace=True)  
    
    df = df.round({"temperature_air": 2, "temperature_internal": 2, "temperature_surface": 2, \
                   "pressure": 2, "pitch": 2, "roll": 2, "heading": 2, "voltage": 2}) 
    
    return(df)


def calculate_velocity(input_data):
    """

    Calculates direction, back azimuth, distance and speed between iceberg positions.

    Parameters
    ----------
    input_data : Pandas dataframe
        A cleaned Pandas dataframe.

    Returns
    -------
    A cleaned Pandas dataframe with calculated velocity information.

    """

    logger.info("Executing: calculate_velocity()")

    # Assign new name to dataframe
    df = input_data

    # Convert datetime
    df["datetime_data"] = pd.to_datetime(df["datetime_data"])

    # Ensure rows are sorted by datetime. Uses datetime_data first and datatime_transmit
    df.sort_values(by="datetime_data", inplace=True)

    # Initialize pyproj with appropriate ellipsoid
    geodesic = pyproj.Geod(ellps="WGS84")

    # Calculate forward azimuth and great circle distance between modelled coordinates
    df["direction"], back_azimuth, df["distance"] = geodesic.inv(
        df["longitude"].shift().tolist(),
        df["latitude"].shift().tolist(),
        df["longitude"].tolist(),
        df["latitude"].tolist(),
    )

    # Convert azimuth from (-180° to 180°) to (0° to 360°)
    df["direction"] = ((df["direction"] + 360) % 360).round(2)

    # Calculate time delta between rows (in seconds)
    df["time_delta"] = df["datetime_data"].diff().dt.total_seconds()

    # Calculate speed in m/s
    df["speed"] = df["distance"] / df["time_delta"]
    
    # Round columns
    #df = df.round({"distance": 1, "speed": 2, "direction": 2}) # Not working?
    df["distance"] = df["distance"].round(2)
    df["direction"] = df["direction"].round(2)
    df["speed"] = df["speed"].round(4)
    
    return(df)


def create_output_files(file, input_data):
    """


    Parameters
    ----------
    input_data : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    logger.info("Executing: create_output_files()")

    # Get unique beacon ID
    filename = Path(file).stem

    # Assign new name to dataframe
    df = input_data

    # -------------------------------------------------------------------------
    # Get standardized data output path
    # -------------------------------------------------------------------------
    path_output = Path(file).resolve().parents[2] / "standardized_data"

    # -------------------------------------------------------------------------
    # Export to CSV
    # -------------------------------------------------------------------------

    # Write CSV file without index column
    df.to_csv("{}/{}.csv".format(path_output, filename), index=False)
    logger.info("Filename: {}.csv".format(filename))
    
    # -------------------------------------------------------------------------
    # Export to shapefile
    # -------------------------------------------------------------------------
    
    # Debugging only
    #df = pd.read_csv("/Volumes/data/iceberg_tracking_beacon_database/data/2023/300434063290950/standardized_data/2023_300434063290950.csv", index_col=False)
    
    # Convert to GeoPandas dataframe
    gdf = gpd.GeoDataFrame(
        df, geometry=gpd.points_from_xy(df["longitude"], df["latitude"])
    )

    # Convert datetime columns to string as ESRI driver does not support datetime fields
    gdf["datetime_data"] = pd.to_datetime(gdf["datetime_data"])
    gdf["datetime_data"] = gdf["datetime_data"].dt.strftime("%Y-%m-%d %H:%M:%S")
    gdf["datetime_transmit"] = pd.to_datetime(gdf["datetime_transmit"])
    gdf["datetime_transmit"] = gdf["datetime_transmit"].dt.strftime("%Y-%m-%d %H:%M:%S")

    # Set CRS
    gdf.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

    # Output shapefile
    gdf.to_file("{}/{}.shp".format(path_output, filename), driver="ESRI Shapefile")
    
    # -------------------------------------------------------------------------
    # Export to KML
    # -------------------------------------------------------------------------
    
    # Enable fiona driver
    gpd.io.file.fiona.drvsupport.supported_drivers['KML'] = 'rw'

    line_gdf = gdf.groupby(['beacon_id'])['geometry'].apply(lambda x: LineString(x.tolist()))

    with fiona.Env():
        gdf.to_file("{}/{}_point.kml".format(path_output, filename), driver="KML")
        line_gdf.to_file("{}/{}_line.kml".format(path_output, filename), driver="KML")
    
    
def create_database(path_input):
    """
    Recursively searches for and concatenantes all standardized CSV files.

    Parameters
    ----------
    path_input : str
        Input path to database.

    Returns
    -------
    None.

    """

    # Get timestamp
    date = datetime.now()
    filename = "database_{}.csv".format(date.strftime("%Y%m%d_%H%M%S"))
    path_output = "{}/release/csv/".format(path_input)

    # Create output path if required
    try:
        Path(path_output).mkdir(parents=True, exist_ok=False)
    except FileExistsError:
        print("{} already exists".format(path_output))
    else:
        print("{} folder was created".format(path_output))

    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*.csv", recursive=True)
    )

    # Concatenate CSV files
    with open(path_output + filename, "w") as outfile:
        for i, file in enumerate(files):
            with open(file, "r") as infile:
                if i != 0:
                    infile.readline()  # Throw away header on all but first file
                # Block copy rest of file from input to output without parsing
                shutil.copyfileobj(infile, outfile)
                print(file + " has been imported.")

    
