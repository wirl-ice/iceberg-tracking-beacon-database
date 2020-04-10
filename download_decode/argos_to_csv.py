# -*- coding: utf-8 -*-
"""

argos2csv.py

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

The datadirectory must contain the file above;
Run py with id and year as arguments
____________________________
How to decode a typical beacon data from Argos:

Each download consists in a 2 lines data set like this:

 12993  55.990N  59.692W   1            043/1909Z-043/1906
  ( 5) 0.92000E+3 ? 0.15000E+2   -.89000E+1           00

Each of the fields is explained below:

12993:	transmitter id
55.990N:	Latitude in thousandths of a degree
59.692W: 	Longitude in thousandths of a degree
1:		Location class (accuracy) 0= >1km, 1= >350m, 2= >150m, 3= <150m.
043/1909Z:	Julian day/time of data collection (satellite overpass)
043/1906:	Julian day/time of beacon location.
( 5):		Message compression index: # of messages received during satellite pass.
0.92000E+3:	Sensor 1; Pressure sensor value (default to 920mb when no sensor available)
0.15000E+2: Sensor 2; Battery voltage 15.0 V (range between 5 - 17.5 V).
-.89000E+1:	Sensor 3; Ambiant air temperature (-8.9 C).
00:		Sensor 4;(not defined for CALIB)
____________________________


 
The output is two parts:
1) a csv file - specific columns can be included/excluded by modifying
the code below.  At present they are:

TransmitterID, WMO, Latitude, Longitude, LocAccuracy, SatelliteTime, BeaconTime, MessageIndex, AtmPress, BattVoltage, AirTemperature
 2) Any notes from the Argos file are output as a separate text file.
"""
    Version History
    Author              Date                Reason
    --------------------------------------------------------------
    Derek Mueller       Feb 2010           Creation
    Derek Mueller       Apr 2010           Mod to accommodate new id = 6 chars.
    Jill Rajewicz       Nov 2017        Jill Rajewicz       Nov 2017	   Modified to output more fields, skip extraneous ?	          
"""

import glob
import sys
import os
import datetime
import numpy


# PARAMETERS - these could be args!
datadir = '/home/wirl/Desktop/CIS/Scripts/'
#id = '1105'  # this is a string - transmitter id number
#year = 2002

def doy2date(year, doy):
    ''' give a float, integer or string and get a datetime'''

    if type(doy) == type(''):
        doy = float(doy)
    if type(year) == type(''):
        year = int(year)
    if type(year) == type(2.0):
        year = int(year)
        print 'Result might be invalid : year coerced to integer'
    return datetime.datetime(year, 1, 1) + datetime.timedelta(doy - 1)


def datetime2iso(datetimeobj):
    ''' return iso string from a python datetime'''
    return datetimeobj.strftime('%Y-%m-%d %H:%M:%S')
	
if __name__ == "__main__":
    
    if len(sys.argv) == 1:
        print '''Please supply the beacon id for the argos beacon you wish to process (not WMO number)
            and then the year to process:  Ex. python argos.py 11257 2009'''           
        sys.exit(1)

    if len(sys.argv) > 1:
        id = sys.argv[1]
        
    if len(sys.argv) == 3:
        year = sys.argv[2]
        
    if len(sys.argv) > 3:
        print 'Too many arguments'
        sys.exit(1)

os.chdir(os.path.join(datadir))

fname = glob.glob(id+'_'+str(year)+'*.txt')
print(len(fname))
if len(fname) != 1:
    print "Ambiguous file specification"
    sys.exit(1)

fname = fname[0]
if not os.path.isfile(fname):
    print "File does not exist"
    sys.exit(1)
    
fname_out = os.path.splitext(fname)[0]+'.csv'
fname_notes = os.path.splitext('notes_'+fname)[0]+'.txt'

# can't seem to do this with iter - so read the whole file:
f = open(fname, mode='r')
fp = f.readlines()
f.close()

# make empty lists to add data to.
TransmitterID = []
Latitude = []
Longitude = []
LocAccuracy = []
SatelliteTime = []
BeaconTime = []

MessageIndex = []
AtmPres = []
BattVoltage = []
AirTemperature = []

comments = []
        
# There are four types of lines - 
    # blank line '\n'
    # begining of the obs - starts with id
    # end of the obs - starts with '('
    # a comment  - none of the above

obs1 = []
obs2 = []
for i, line in enumerate(fp):    
    fields = line.split()
    if len(fields) == 0:
        continue
    elif id in fields[0]:
        if fields[1] != 'NO':  #sometimes NO LOCATION happens... (just remove)
            obs1.append(i)
    elif '(' in fields[0]:  #hope that no comment lines start with '('
        obs2.append(i)

for i, line in enumerate(fp):
    fields = line.split()
    if len(fields) == 0: 
        continue 
    elif id in fields[0] and fields[1] != 'NO' and i+1 in obs2:
        # beacon ID
        beaconID.append(fields[0])
      
        # Lat and Lon:
        for i in range(1,3):
            strLoc = fields[i]
            if strLoc[-1:] == 'S' or strLoc[-1:] == 'W':
                numLoc = float(strLoc[:-1])*-1
            elif strLoc[-1:] == 'N' or strLoc[-1:] == 'E':
                numLoc = float(strLoc[:-1])
            else:
                print line
            if i == 1:
                lat.append(numLoc)
            else:
                lon.append(numLoc)
        # location quality
        locQual.append(int(fields[3]))
        
        #dates/times
        trans, gps = fields[4].split('-')
        # trans time is time of transmission
        doy_trans, hr_trans = trans.split('/')
        date_trans = doy2date(year, doy_trans)
        hr_trans = datetime.time( int(hr_trans[:2]),  int(hr_trans[2:-1]) )
        date_trans = date_trans.combine(date_trans, hr_trans)
        SatelliteTime.append(datetime2iso(date_trans))
        
        # gps time is time of position
        doy_gps, hr_gps = gps.split('/')
        date_gps = doy2date(year, doy_gps)
        hr_gps = datetime.time( int(hr_gps[:2]),  int(hr_gps[2:]) )
        date_gps = date_gps.combine(date_gps, hr_gps)
        BeaconTime.append(datetime2iso(date_gps))
        
    elif '(' in fields[0] and i-1 in obs1:
        if fields[0] == '(':
            MessageIndex.append(int(fields[1][:-1]))
            AtmPress.append(float(fields[2])) # check units 
            BattVoltage.append(float(fields[3]))
            AirTemperature.append(float(fields[4]))
        else: # hope there are no exceptions to this rule
            MessageIndex.append(int(fields[0][1:-1]))
            AtmPress.append(float(fields[1])) # check units 
            BattVoltage.append(float(fields[2]))
            AirTemperature.append(float(fields[3]))
    
    else:    
        comments.append(line)

# test to make sure that all the field lists are the same length
datacheck = numpy.array([len(TransmitterID), len(Latitude), len(Longitude), len(LocAccuracy), 
                    len(SatelliteTime), len(BeaconTime), len(MessageIndex), len(AtmPress), 
                    len(BattVoltage), len(AirTemperature)])

if datacheck.min() != datacheck.max():
    print 'Error:  The fields had different numbers of observations'
    sys.exit(1)

WMO=[]

for i in range(1, datacheck.max()+1):
    WMO.append(WMO_num)

#observation number = comment out if don't want this weird roW!
#obs = []
#for i in range(1, datacheck.max()+1):
    #obs.append(i)
    
#ok now make a big array with all data
     #first option here is including ALL possible fields - I want to keep all info at this point
beacon = numpy.asarray([TransmitterID, WMO, Latitude, Longitude, LocAccuracy, SatelliteTime, BeaconTime, MessageIndex, AtmPress, BattVoltage, AirTemperature])
#beacon = numpy.asarray([beaconID, lat, lon, locQual, gps_time, atmPres, batt, airTemp])
beacon = numpy.transpose(beacon)

arr_format = str(beacon.dtype)[1:]
arr_format = '%'+arr_format[1:]+arr_format[:1].lower()
numpy.savetxt(fname_out, beacon, fmt=arr_format, delimiter=',', header= 'TransmitterID, WMO, Latitude, Longitude, LocAccuracy, SatelliteTime, BeaconTime, MessageIndex, AtmPress, BattVoltage, AirTemperature', comments='')  


# now save the notes in a text file
fp = open(fname_notes, 'w')

for note in comments:
    fp.write(note), 
    
fp.close()
