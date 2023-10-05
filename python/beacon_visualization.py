#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 11 11:04:12 2022

@author: Adam Garbo

@description: Functions to produce visualizations of beacon data that
has been ingested, cleaned, and standardized. 
Functions will recursively search through all standardized data folders and
produce visualizations including a map of the beacon trajector, annotated 
with key metrics (e.g., time, distance), as well as plots of key variables 
such as speed, distance, and temperature.


"""

import logging
import glob
from pathlib import Path
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.dates as mdates
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import pyproj
from pyproj import Proj


# -----------------------------------------------------------------------------
# Configure logging
# -----------------------------------------------------------------------------

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
formatter = logging.Formatter(
    "%(asctime)s:%(msecs)d %(name)s %(levelname)s %(message)s", "%Y-%m-%d %H:%M:%S"
)

# -----------------------------------------------------------------------------
# Plotting attributes
# -----------------------------------------------------------------------------

# Seaborn configuration
sns.set_theme(style="ticks")
sns.set_context("talk") # talk, paper, poster

# Set colour palette
sns.set_palette("colorblind")

# Graph attributes
lw = 1
interval = 30
date_format = "%Y-%m-%d"

# Figure DPI
dpi = 300

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


# -----------------------------------------------------------------------------
# Folder paths
# -----------------------------------------------------------------------------

# Specify input path (to do: make into an argument)
path_input = "/Volumes/data/iceberg_beacon_database"

# Removed data only
path_input = "/Volumes/data/iceberg_beacon_database/removed_data"

# Local testing
path_input = "/Users/adam/Desktop/iceberg_beacon_database"




# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------

visualize_maps(path_input)
visualize_graphs(path_input)

def visualize_maps(path_input):

    logger.info("Executing: visualize_data()")

    # Recursively search for all files to be processed
    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*.csv", recursive=True)
    )

    # Process all files
    for file in files:

        # Get standardized data output path
        path_output = Path(file).resolve().parents[0]
        
        # Debugging only
        path_output = "/Users/adam/Desktop/iceberg-beacon-database/figures"
        
        # Get unique beacon ID
        filename = Path(file).stem

        print("Visualizing {}".format(filename))

        # Debugging only
        #file = "/Users/adam/Desktop/iceberg-beacon-database/2000_11254.csv"

        # Load standardized CSV file
        df = pd.read_csv(file, index_col=False)
        
        # Convert datetime
        df["datetime_data"] = pd.to_datetime(df["datetime_data"])

        # Set map centres
        x = df["longitude"].median()
        y = df["latitude"].median()

        # Plot latitude and longitude
        plt.figure(figsize=(10, 10))
        ax = plt.axes(projection=ccrs.Orthographic(x,y))
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
        
        # Calculate variables for annotations
        beacon_type = df["beacon_type"][0]
        beacon_id = df["beacon_id"][0]
        start_date = df["datetime_data"].iloc[0]
        end_date = df["datetime_data"].iloc[-1]
        duration = df["datetime_data"].iloc[-1] - df["datetime_data"].iloc[0]
        distance = df["distance"].sum() / 1000
        
        ax.text(-0.125, 1.025, "%s %s\nStart: %s End: %s\nDuration: %s Distance: %d km" % (beacon_type, beacon_id, start_date, end_date, duration, distance),
        #verticalalignment='top', horizontalalignment='left',
        transform=ax.transAxes,
        color='black', fontsize=18)

        # ax.get_legend().remove()
        plt.savefig(
            "{}/{}.png".format(path_output, filename),
            dpi=dpi,
            transparent=False,
            bbox_inches="tight",
        )
        plt.close()
        
        # Debugging to run function only once
        #break
        
def visualize_graphs(path_input):

    logger.info("Executing: visualize_data()")
    
    # Recursively search for all files to be processed (glob)
    files = sorted(
        glob.glob(path_input + "/**/standardized_data/*.csv", recursive=True)
    )
    
    # Process all files
    for file in files:

        # Get standardized data output path
        path_output = Path(file).resolve().parents[0]
        
        path_output = "/Users/adam/Desktop/iceberg_beacon_database/figures"
        
        # Get unique beacon ID
        filename = Path(file).stem

        df = pd.read_csv("/Users/adam/Desktop/iceberg-beacon-database/2000_11254.csv")

        print("Visualizing {}".format(filename))

        # Load standardized CSV file
        df = pd.read_csv(file, index_col=False)

        # Convert datetime
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
