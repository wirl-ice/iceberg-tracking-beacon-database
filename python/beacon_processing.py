#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 10:59:44 2022

@title: Compilation and standardization of iceberg tracking beacon data
@author: Adam Garbo


"""

import csv
from datetime import datetime
import logging
import glob
import shutil
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.dates as mdates
import seaborn as sns
import numpy as np
from pathlib import Path
import pyproj
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# Add Natural Earth coastline
coast = cfeature.NaturalEarthFeature(
    "physical", "land", "10m", edgecolor="black", facecolor="lightgray", lw=0.5
)

# Add Natural Earth coastline
coastline = cfeature.NaturalEarthFeature(
    "physical", "coastline", "10m", edgecolor="black", facecolor="none", lw=0.75
)

# Seaborn configuration
sns.set_theme(style="ticks")
sns.set_context("talk")  # talk, paper, poster

# Global plot parameters
# plt.rc("legend",fancybox=False, framealpha=1, edgecolor="black")

# Set colour palette
# sns.palplot(sns.color_palette("colorblind"))
# colours = sns.color_palette("colorblind", 10).as_hex()
# sns.set_palette("colorblind", 10)


# -----------------------------------------------------------------------------
# Plotting attributes
# -----------------------------------------------------------------------------

lw = 1
interval = 1
date_format = "%Y-%m-%d"

# Figure DPI
dpi = 200

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

# Specify input path (to do: make into an argument)
path_input = "/Volumes/data/iceberg_beacon_database"

path_input = "/Users/adam/Desktop/iceberg_beacon_database"

# -----------------------------------------------------------------------------
# Configure logging
# -----------------------------------------------------------------------------

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
formatter = logging.Formatter(
    "%(asctime)s:%(msecs)d %(name)s %(levelname)s %(message)s", "%Y-%m-%d %H:%M:%S"
)

# -----------------------------------------------------------------------------
# Beacon deployment test files to confirm function operation
# -----------------------------------------------------------------------------

# BIO
file = "/Volumes/data/iceberg_beacon_database/data/2009/300034012616000/raw_data/deployment_file/2009_300034012616000.csv"

# Cryologger
file = "/Volumes/data/iceberg_beacon_database/data/2018/300434063415160/raw_data/deployment_file/2018_300434063415160.csv"

# CALIB ARGOS
file = "/Volumes/data/iceberg_beacon_database/data/2009/16795/raw_data/deployment_file/2009_16795.csv"

# CALIB Iridium
file = "/Volumes/data/iceberg_beacon_database/data/2014/300234061763040/raw_data/deployment_file/2014_300234061763040.csv"

# Canatec
file = "/Volumes/data/iceberg_beacon_database/data/2009/26973/raw_data/deployment_file/2009_26973.csv"

# CCG
file = "/Volumes/data/iceberg_beacon_database/data/2011/300034013458130/raw_data/deployment_file/2011_300034013458130.csv"

# IABP
file = "/Volumes/data/iceberg_beacon_database/data/2016/300234062950220/raw_data/deployment_file/2016_300234062950220.csv"

# IIP
file = "/Volumes/data/iceberg_beacon_database/data/2019/3037-2674613/raw_data/deployment_file/2019_3037-2674613.csv"

# Navidatum
file = "/Volumes/data/iceberg_beacon_database/data/2012/100000000000000/raw_data/deployment_file/2012_100000000000000.csv"

# Oceanetic
file = "/Volumes/data/iceberg_beacon_database/data/2011/300034013463170/raw_data/deployment_file/2011_300034013463170.csv"

# RockSTAR
file = "/Volumes/data/iceberg_beacon_database/data/2016/300234060172440/raw_data/deployment_file/2016_300234060172440.csv"

# Solara
file = "/Volumes/data/iceberg_beacon_database/data/2018/300234066241900/raw_data/deployment_file/2018_300234066241900.csv"

# SVP
file = "/Volumes/data/iceberg_beacon_database/data/2015/300234060104820/raw_data/deployment_file/2015_300234060104820.csv"

# Load test data
raw_data = pd.read_csv(file, index_col=False, skipinitialspace=True)

# Display columns
raw_data.columns

# -----------------------------------------------------------------------------
# Execute function(s)
# -----------------------------------------------------------------------------

# Process all data
process_data(path_input)

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

    # Process all files
    for file in files:

        # Get standardized data output path
        path_output = Path(file).resolve().parents[2] / "standardized_data"

        """
        # Delete existing files in output path
        files = glob.glob(str(path_output) + "/*")
        for f in files:
            try:
                os.remove(f)
            except OSError:
                pass
        """

        # Get unique beacon ID
        filename = Path(file).stem

        # Set log file path and name
        logfile = "{}/debug_{}.log".format(path_output, filename)

        # Create file handler and set formatter
        file_handler = logging.FileHandler(logfile, mode="w")
        file_handler.setFormatter(formatter)

        # Add handler to the logger
        logger.addHandler(file_handler)

        logger.info("Processing {}".format(file))
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
    df.loc[(df["ta"] >= ta_max) | (df["ta"] <= ta_min), "ta"] = np.nan

    # Internal temperature
    df.loc[(df["ti"] >= ti_max) | (df["ti"] <= ti_min), "ti"] = np.nan

    # Surface temperature
    df.loc[(df["ts"] >= ts_max) | (df["ts"] <= ts_min), "ts"] = np.nan

    # Pressure
    df.loc[(df["bp"] >= bp_max) | (df["bp"] <= bp_min), "bp"] = np.nan

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
    df.loc[(df["vbat"] >= vbat_max) | (df["vbat"] <= vbat_min), "vbat"] = np.nan

    # Drop all rows where latitude or longitude is nan
    df.dropna(subset=["latitude", "longitude"], inplace=True)
    
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
    df["direction"] = (df["direction"] + 360) % 360

    # Calculate time delta between rows (in seconds)
    df["time_delta"] = df["datetime_data"].diff().dt.total_seconds()

    # Calculate speed in m/s
    df["speed"] = df["distance"] / df["time_delta"]
    
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
    df.to_csv("{}/{}_test.csv".format(path_output, filename), index=False)

    # -------------------------------------------------------------------------
    # Export to shapefile
    # -------------------------------------------------------------------------

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
    path_output = "{}/output_data/csv/".format(path_input)

    # Create output path if required
    try:
        Path(path_output).mkdir(parents=True, exist_ok=False)
    except FileExistsError:
        print("{} already exists".format(path_output))
    else:
        print("{} folder was created".format(path_output))

    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*_test.csv", recursive=True)
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


def visualize_maps(path_input):

    logger.info("Executing: visualize_data()")

    # Recursively search for all files to be processed
    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*.csv", recursive=True)
    )

    # Process all files
    for file in files:

        # Get standardized data output path
        #path_output = Path(file).resolve().parents[0]
        
        path_output = "/Users/adam/Desktop/iceberg_beacon_database/figures"
        
        # Get unique beacon ID
        filename = Path(file).stem

        print("Visualizing {}".format(filename))

        # Load standardized CSV file
        df = pd.read_csv(file, index_col=False)

        # Plot latitude and longitude
        plt.figure(figsize=(10, 10))
        ax = plt.axes(projection=ccrs.PlateCarree())
        ax.add_feature(coast)
        ax.set_adjustable("datalim")
        gl = ax.gridlines(
            crs=ccrs.PlateCarree(),
            draw_labels=True,
            color="black",
            alpha=0.25,
            linestyle="dotted",
            x_inline=False,
            y_inline=False,
        )
        gl.rotate_labels = False
        gl.top_labels = False
        gl.right_labels = False

        gl.xpadding = 5
        sns.scatterplot(
            x="longitude",
            y="latitude",
            data=df,
            linewidth=1,
            edgecolor="black",
            transform=ccrs.PlateCarree(),
        )
        # ax.get_legend().remove()
        plt.savefig(
            "{}/{}.png".format(path_output, filename),
            dpi=dpi,
            transparent=False,
            bbox_inches="tight",
        )
        plt.close()


        # Daily displacement
        fig, ax = plt.subplots(figsize=(10,5))
        ax.grid(ls="dotted")
        sns.lineplot(x="datetime_data", y="distance", data=df, errorbar=None)
        ax.set(xlabel=None, ylabel="Speed (m/s)")
        plt.xticks(rotation=45, horizontalalignment="center")
        #ax.xaxis.set_major_locator(mdates.MonthLocator(interval=interval))
        sns.despine()
        #ax.legend(loc="center", bbox_to_anchor=(0.5, -0.35), ncol=2)
        plt.savefig(
            "{}/{}_speed.png".format(path_output, filename),
            dpi=dpi,
            transparent=False,
            bbox_inches="tight",
        )
        plt.close()
        # Debugging
        #break
        
        


df = pd.read_csv("/Users/adam/Desktop/iceberg_beacon_database/data/2010/11256/standardized_data/2010_11256_test.csv", index_col=False)


visualize_graphs(path_input)

def visualize_graphs(path_input):

    logger.info("Executing: visualize_data()")

    # Recursively search for all files to be processed
    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*.csv", recursive=True)
    )

    # Process all files
    for file in files:

        # Get standardized data output path
        #path_output = Path(file).resolve().parents[0]
        
        path_output = "/Users/adam/Desktop/iceberg_beacon_database/figures"
        
        # Get unique beacon ID
        filename = Path(file).stem

        print("Visualizing {}".format(filename))

        # Load standardized CSV file
        df = pd.read_csv(file, index_col=False)

        df["datetime_data"] = pd.to_datetime(df["datetime_data"])
    
    
        try:
            # Daily displacement
            fig, ax = plt.subplots(figsize=(10,5))
            ax.grid(ls="dotted")
            sns.lineplot(x="datetime_data", y="speed", data=df, errorbar=None)
            ax.set(xlabel=None, ylabel="Speed (m/s)")
            plt.xticks(rotation=45, horizontalalignment="center")
            #ax.xaxis.set_major_locator(mdates.MonthLocator(interval=interval))
            sns.despine()
            #ax.legend(loc="center", bbox_to_anchor=(0.5, -0.35), ncol=2)
            plt.savefig(
                "{}/{}_speed.png".format(path_output, filename),
                dpi=dpi,
                transparent=False,
                bbox_inches="tight",
            )
            plt.close()
        except Exception:
            pass

        # Debugging
        #break