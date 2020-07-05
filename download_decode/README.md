# Download & Decode

## Introduction
This collection of code is used to query an email account and download and decode SBD messages from MetOcean and Oceanetics SBD beacons. 

## Description
Contains: 
* download_and_decode.py
* sbd_decoder.py
* metocean_decoder.py
* oceanetic_decoder.py
* paths.py

download_and_decode.py is the parent script, and calls on paths.py, sbd_decoder.py,metocean_decoder.py, and oceanetic_decoder.py as required. 

Written in python by Derek Mueller and Cindy Lopes in 2012. Modified by Jill Rajewicz April 2018.

Note that password and username for email account has been redacted and would need to be filled in to use.

## Usage

argos_to_csv.py
----------------

This script handles raw ARGOS transmissions (see below) and converts them to a 
csv file for further processing using R

Input: An Argos text file from CIS, renamed as follows:

beaconID_year.ext
11257_2009.txt

To use this script, Argos file input must have already been cleaned - any entries where lines have ??? for
the transmission data should be removed. These entries occur where the overhead satellites 
did not receive enough beeps from the beacon to register a new position. These occur when 
a beacon is first turned on, until such time that the beacon sends its first set of signals 
in its new location.

Entries must also have been converted from hexidecimal format and decoded using the multipliers 
specified by Argos before using this script.

Run in py with id and year as arguments

The output is a csv file with the column headings
TransmitterID, WMO, Latitude, Longitude, LocAccuracy, SatelliteTime, BeaconTime, MessageIndex, AtmPress, BattVoltage, AirTemperature

Any notes from Argos text file are output separately as a text file.
