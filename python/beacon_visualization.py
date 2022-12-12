#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 11 11:04:12 2022

@author: adam
"""

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
# Configuration
# -----------------------------------------------------------------------------

# Add Natural Earth coastline
coast = cfeature.NaturalEarthFeature("physical", "land", "10m",
                                     edgecolor="black",
                                     facecolor="lightgray",
                                     lw=0.5)

# Add Natural Earth coastline
coastline = cfeature.NaturalEarthFeature("physical", "coastline", "10m",
                                         edgecolor="black",
                                         facecolor="none",
                                         lw=0.75)

# Seaborn configuration
sns.set_theme(style="ticks")
sns.set_context("talk") # talk, paper, poster

# Global plot parameters
#plt.rc("legend",fancybox=False, framealpha=1, edgecolor="black")

# Set colour palette
#sns.palplot(sns.color_palette("colorblind"))
#colours = sns.color_palette("colorblind", 10).as_hex()
#sns.set_palette("colorblind", 10)


# -----------------------------------------------------------------------------
# Plotting attributes
# -----------------------------------------------------------------------------

lw = 1
interval = 1
date_format = "%Y-%m-%d"

# Figure DPI
dpi = 300

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------

df1 = pd.read_csv("/Users/adam/Desktop/iceberg_beacon_database/data/2010/11256/standardized_data/2010_11256_test.csv", index_col=False)
df2 = pd.read_csv("/Users/adam/Desktop/iceberg_beacon_database/data/2010/11256/standardized_data/2010_11256.csv", index_col=False)


df1 = pd.read_csv("/Users/adam/Desktop/iceberg_beacon_database/data/2010/300034012592660/standardized_data/2010_300034012592660_test.csv", index_col=False)
df2 = pd.read_csv("/Users/adam/Desktop/iceberg_beacon_database/data/2010/300034012592660/standardized_data/2010_300034012592660.csv", index_col=False)


# Convert datetimes
df1["datetime_data"] = pd.to_datetime(df1["datetime_data"].astype(str), format="%Y-%m-%d")
df2["datetime_data"] = pd.to_datetime(df2["datetime_data"].astype(str), format="%Y-%m-%d")


# -----------------------------------------------------------------------------
# Plots - Speed & Distance
# -----------------------------------------------------------------------------

# Latitude and longitude
fig, ax = plt.subplots(figsize=(10,5))
ax.grid(ls="dotted")
sns.scatterplot(x="longitude", y="latitude", data=df1, label="df1")
sns.scatterplot(x="longitude", y="latitude", data=df2, label="df2")
ax.set(xlabel=None)
plt.xticks(rotation=45, horizontalalignment="center")
ax.legend(loc="center", bbox_to_anchor=(0.5, -0.35), ncol=2)


# Temperature
fig, ax = plt.subplots(figsize=(10,5))
ax.grid(ls="dotted")
sns.lineplot(x="datetime_data", y="ti", data=df1, errorbar=None, label="df1")
sns.lineplot(x="datetime_data", y="ti", data=df2, errorbar=None, label="df2")
ax.set(xlabel=None)
plt.xticks(rotation=45, horizontalalignment="center")
ax.legend(loc="center", bbox_to_anchor=(0.5, -0.35), ncol=2)
