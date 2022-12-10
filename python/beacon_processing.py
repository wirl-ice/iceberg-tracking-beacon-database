#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 10:59:44 2022

@title: Compilation and standardization of iceberg tracking beacon data
@author: Adam Garbo


"""

from process_scripts import *


import glob
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.dates as mdates
import seaborn as sns
import numpy as np
from pathlib import Path


# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

# Specify input path (can be an argument)
path_input = "/Users/adam/Desktop/data/"


# -----------------------------------------------------------------------------
# Standardized column names
# -----------------------------------------------------------------------------

columns = ["beacon_id", "beacon_type", "datetime_data", "datetime_transmit", 
           "latitude", "longitude", "vbat", "ta", "ti", "ts", "bp", 
           "pitch", "roll", "heading", "satellites", "loc_accuracy", 
           "message_index", "gps_delay", "snr", "ttff"]

# -----------------------------------------------------------------------------
# Execute functions
# -----------------------------------------------------------------------------

#
process_data(path_input)

#
clean_data()

#
create_output_files()


# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------

def process_data(path_input):
    
    # Recurisively search for all files to be processed
    files = sorted(glob.glob(path_input + "/**/raw_data/deployment_file/*.csv", recursive=True))
    
    for file in files: 
        
        # Get unique beacon ID
        filename = Path(file).stem
        
        # Load beacon deployment file CSV
        raw_data = pd.read_csv(file, index_col=False)
        
        # Get appropriate function
        function_to_call = get_function(filename)
        
        # Process beacon deployment file
        function_to_call(filename, raw_data)
    
    



def clean_data(data):
    
    return(processed_data)


def calculate_speed(input_data):

    # Assign new name to dataframe
    df = input_data
    
    # Initialize pyproj with appropriate ellipsoid
    geodesic = pyproj.Geod(ellps="WGS84")
    
    # Calculate forward azimuth and great circle distance between modelled coordinates
    df["direction"], back_azimuth, df["distance"] = geodesic.inv(df["longitude"].shift().tolist(), 
                                                                 df["latitude"].shift().tolist(),
                                                                 df["longitude"].tolist(), 
                                                                 df["latitude"].tolist())
    
    # Convert azimuth from (-180° to 180°) to (0° to 360°)
    df["direction"] = (df["direction"] + 360) % 360
    
    
    df['time_delta'] = pd.to_timedelta(df['datetime_data'].astype(str)).diff(-1).dt.total_seconds().div(60)
    
    df["speed"] = 
    
    
    return(df)

def create_output_files(input_data):
    
    # Export to CSV
    df2.to_csv("{}{}_stats.csv".format(path_output,filename))
    
    # Choose file name
    output_file = filename
    
    # Choose output directory 
    setwd(output)
    write.csv(standardized_data, paste(output_file, ".csv", sep = ""), row.names=FALSE)
  
    
def get_function(beacon_id):

    function_dict = {
        "1997_12995":process_calib_argos,
        "2002_1104":process_calib_argos,
        "2003_11247":process_calib_argos,
        "2004_16795":process_calib_argos,
        "2007_11251":process_calib_argos,
        "2008_11255":process_calib_argos,
        "2008_16794":process_calib_argos,
        "2008_300034012610510":process_bio,
        "2009_11257":process_calib_argos,
        "2009_12996":process_calib_argos,
        "2009_16792":process_calib_argos,
        "2009_16795":process_calib_argos,
        "2009_26973":process_calib_argos,
        "2009_300034012519990":process_bio,
        "2009_300034012571050":process_canatec,
        "2009_300034012616000":process_bio,
        "2010_11256":process_calib_argos,
        "2010_12993":process_calib_argos,
        "2010_12994":process_calib_argos,
        "2010_300034012592660":process_canatec,
        "2010_93693":process_calib_argos,
        "2011_11247":process_calib_argos,
        "2011_12995":process_calib_argos,
        "2011_12997":process_calib_argos,
        "2011_300034012484860":process_bio,
        "2011_300034012489850":process_bio,
        "2011_300034013149880":process_ccg,
        "2011_300034013439530":process_ccg,
        "2011_300034013453190":process_ccg,
        "2011_300034013453240":process_ccg,
        "2011_300034013458130":process_ccg,
        "2011_300034013463170":process_oceanetic,
        "2011_300234010031950":process_svp,
        "2011_300234010033940":process_svp,
        "2011_300234010035940_PII-A":process_svp,
        "2011_300234010035940_PII-B":process_svp,
        "2011_300234010955690":process_svp,
        "2011_300234010955700":process_svp,
        "2011_300234010958690_PII-A":process_svp,
        "2011_300234010958690_PII-B":process_svp,
        "2011_300234010959690":process_svp,
        "2012_1000000000000000":process_navidatum,
        "2012_11248":process_calib_argos,
        "2012_11249":process_calib_argos,
        "2012_11251":process_calib_argos,
        "2012_11252":process_calib_argos,
        "2012_300034012480920":process_bio,
        "2012_300234010082470":process_svp,
        "2012_300234010132070":process_svp,
        "2013_300034013460170":process_oceanetic,
        "2013_300034013461170":process_oceanetic,
        "2013_300034013464160":process_oceanetic,
        "2013_300034013464170":process_oceanetic,
        "2013_300034013468160":process_oceanetic,
        "2013_300234011240410":process_canatec,
        "2013_300234011241410":process_canatec,
        "2013_300234011242410":process_canatec,
        "2013_300234011938510":process_canatec,
        "2014_300234060544160":process_svp,
        "2014_300234061763040":process_calib_iridium,
        "2015_300134010204980":process_solara,
        "2015_300134010505190":process_solara,
        "2015_300134010906790":process_solara,
        "2015_300134010907780":process_solara,
        "2015_300234060104820":process_svp,
        "2015_300234060435010":process_svp,
        "2015_300234061762030":process_calib_iridium,
        "2015_300234062790480":process_canatec,
        "2015_300234062791420":process_canatec,
        "2015_300234062791700":process_canatec,
        "2015_300234062792490":process_canatec,
        "2015_300234062792500":process_canatec,
        "2015_300234062794470":process_canatec,
        "2015_300234062795460":process_canatec,
        "2015_300234062795640":process_canatec,
        "2015_300234062796490":process_canatec,
        "2015_300234062796640":process_canatec,
        "2016_300234060172440":process_rockstar,
        "2016_300234060172670":process_rockstar,
        "2016_300234061761040":process_calib_iridium,
        "2016_300234061763030":process_calib_iridium,
        "2016_300234061768060":process_calib_iridium,
        "2016_300234062950220":process_iabp,
        "2016_300234062951220":process_iabp,
        "2016_300234062957250":process_iabp,
        "2016_300234063513450":process_calib_iridium,
        "2016_300234063515450":process_calib_iridium,
        "2016_300234064706730":process_rockstar,
        "2016_300234064808170":process_rockstar,
        "2016_300234064808210":process_rockstar,
        "2016_300434063218800":process_rockstar,
        "2016_300434063417140":process_rockstar,
        "2017_300234010025000":process_rockstar,
        "2017_300234060169280":process_rockstar,
        "2017_300234060177480":process_rockstar,
        "2017_300234060270020":process_rockstar,
        "2017_300234060272000":process_rockstar,
        "2017_300234060276010":process_rockstar,
        "2017_300234060563100":process_rockstar,
        "2017_300234060692710":process_rockstar,
        "2017_300234060699700":process_rockstar,
        "2017_300234062324750":process_svp,
        "2017_300234062325760":process_svp,
        "2017_300234062327750":process_svp,
        "2017_300234062328750":process_svp,
        "2017_300234063516450":process_calib_iridium,
        "2018_300234060362670":process_rockstar,
        "2018_300234060367670":process_rockstar,
        "2018_300234062807520":process_rockstar,
        "2018_300234066241900":process_solara,
        "2018_300234066549270":process_solara,
        "2018_300434063411050":process_cryologger,
        "2018_300434063415110":process_cryologger,
        "2018_300434063415160":process_cryologger,
        "2018_300434063416060":process_cryologger,
        "2018_300434063418130":process_cryologger,
        "2018_300434063419120":process_cryologger,
        "2019_1012-2669855":process_iip,
        "2019_1027-2670078":process_iip,
        "2019_1124-2670293":process_iip,
        "2019_2001-2632176":process_iip,
        "2019_2009-2632235":process_iip,
        "2019_2011-2632168":process_iip,
        "2019_2013-2670502":process_iip,
        "2019_2015-2671136":process_iip,
        "2019_2020-2671162":process_iip,
        "2019_2022-2670073":process_iip,
        "2019_2030-2670510":process_iip,
        "2019_2040-2670142":process_iip,
        "2019_2045-2670074":process_iip,
        "2019_300234060206850":process_rockstar,
        "2019_300234060370740":process_rockstar,
        "2019_300234060698700":process_rockstar,
        "2019_300234063265700":process_cryologger,
        "2019_300234065254740":process_cryologger,
        "2019_300434063392070":process_cryologger,
        "2019_300434063394110":process_cryologger,
        "2019_300434063494100":process_cryologger,
        "2019_300434063495310":process_cryologger,
        "2019_3037-2674613":process_iip,
        "2019_4013-2674911":process_iip,
        "2019_4022-2674941":process_iip,
        "2019_4025-2678597":process_iip,
        "2019_4027-2670071":process_iip,
        "2019_4028-2671160":process_iip,
        "2021_300434065734810":process_cryologger,
        "2021_300434065732760":process_cryologger,
        "2021_300434065868240":process_cryologger,
        "2021_300434065860260":process_cryologger,
        "2021_300434065869240":process_cryologger,
        "2021_300434065860320":process_cryologger,
        "2021_300434065864290":process_cryologger,
        "2021_300434065861350":process_cryologger,
        "2021_300434063291950":process_cryologger,
    }
    return(function_dict[beacon_id])