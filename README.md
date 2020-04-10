# Canadian Ice Service (CIS) Iceberg Tracking Beacon Database

Date: 2020-04-10

## Introduction
The Canadian Ice Service has recently compiled one of the most comprehensive databases of in-situ iceberg tracking beacon drift trajectories in the Northern Hemisphere, with over 800,00 observations collected between 1997 and 2019. The database contains iceberg and ice island position and meteorological data as measured by tracking beacons as well as information about the target at the time of deployment (e.g. shape, dimensions, source, thickness) where possible. Data have been contributed by government, academic, and industry sources from tracking beacons deployed on targets in the western and eastern Canadian Arctic, and off the east coast of Canada. Drift direction, speed, and pattern data from satellite tracking beacons deployed on icebergs and ice islands will be used to understand how icebergs drift, and to develop and validate models of iceberg drift, in order to improve predictions of ice hazard occurrence and behaviour. 



Explanation of scripts used to retrieve and process data for addition to the beacon database:

Folders:

1) beacon_download:

This collection of code is used to query an email account and download and decode SBD messages from MetOcean and Oceanetics SBD beacons. 

Contains:
	Download_and_Decode.py
	sbdDecoder.py
	metOceanDecoder.py
	oceaneticDecoder.py
	paths.py

Download_and_Decode.py is the parent script, and calls on: paths.py, sbdDecoder.py,metOceanDecoder.py, and oceaneticDecoder.py as required. 

Written in Python by Derek Mueller and Cindy Lopes (2012). Modified by Jill Rajewicz (April, 2018).

Note: The password and username for email account has been redacted and would need to be filled in to use.

2) beacon_clean

This suite of code digests raw tracking beacon data in csv format to output a standardized csv file, does some basic cleaning, and outputs other quality added
file types - point and line kml files, point and line shape files, and gpx files.
There is a detailed readme file in the folder which explains all the individual scripts in more detail.

3) beacon_argos

This folder contains the script used to decode Argos text files and convert to a csv, to be further processed into a standardized csv and value added files.
