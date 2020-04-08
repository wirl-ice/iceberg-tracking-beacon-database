Date: 2018-04-27

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
