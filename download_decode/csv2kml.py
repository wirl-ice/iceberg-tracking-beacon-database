#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
csv2kml.py

Created on Thu Aug 12 11:13:26 2021

Quick and dirty way to get all csvs in a folder into kml files

TODO: complete this - not at all ready for primetime... 

@author: dmueller
"""

import os
import fiona
import geopandas as gpd
import pandas as pd
from shapely.geometry import LineString
import glob
#import argparse
from paths import csvPath
fiona.supported_drivers['KML'] = 'rw'


# parser = argparse.ArgumentParser()
# parser.add_argument("-d", "--dir", help="input directory to search through")
# args = parser.parse_args()

os.chdir(csvPath)

files = glob.glob('*.csv')

for file in files: 
    berg = pd.read_csv(file)
    berg = berg.sort_values(by=['Year','Month', 'Day', 'Hour','Minute'])
    
    berg_pt = gpd.GeoDataFrame(berg,  
            geometry=gpd.points_from_xy(berg.Longitude, berg.Latitude), 
            crs="EPSG:4326")
    berg_pt.to_file(file[:-4]+'_wpt.kml',driver="KML")
    
    berg_ln=gpd.GeoDataFrame({'imei':[berg.IMEI[0]],
                          'geometry':[LineString(zip(berg.Longitude, berg.Latitude))]},
                           crs='EPSG:4236')
    berg_ln.to_file(file[:-4]+'_ln.kml',driver="KML")
    
    #KML writing is a bit flaky... .