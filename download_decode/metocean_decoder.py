'''
Script takes an IMEINNumber and data from a cooresponding *.sbd file for a metaOcean beacon.

It extracts the information form the data, creates or appends to a *.csv file with the form ######_yyyy.cvs (# is the last 6 digits of the IMEI Number)
in the following format:

IMEI	        Year	Month	Day	Hour	Minute	Latitude    Longitude   Temperature  Voltage Battery	AtmPress    FormatID
3.00234E+14	2011	10	19	15	0	68.1728	    -61.7342	0.9	     14	                -99	    28

'''

import binascii
import numpy
import csv
import os

from paths import csvPath

def main(IMEINumber, sbd_raw):

    
    file_size = 21

    sbd_hex = binascii.b2a_hex(sbd_raw)
    sbd_bin = bin(int(sbd_hex, 16))[2:]
    n_pad = file_size*8 - len(sbd_bin)
    sbd_bin = '0'*n_pad + sbd_bin


    #splice up based on position and set to cooresponding variables
    formatID = dec(sbd_bin[0:8])
    yyyy = dec(sbd_bin[8:15]) +2000
    mm = dec(sbd_bin[15:19])
    dd = dec(sbd_bin[19:25])
    HH = dec(sbd_bin[25:30])
    MM = dec(sbd_bin[30:36])
    sp1 = dec(sbd_bin[36:47])
    temp = dec(sbd_bin[47:57])*0.1 - 60
    sp2 = dec(sbd_bin[57:66])
    sp3 = dec(sbd_bin[66:76])
    batt = dec(sbd_bin[76:82])*0.2 + 5
    sbd_time = dec(sbd_bin[82:90])
    sp4 = dec(sbd_bin[90:98])
    timelastfix = dec(sbd_bin[98:110])
    lat = dec(sbd_bin[110:130])*0.0002 - 90
    lon = dec(sbd_bin[130:151])*0.0002 - 180
    timefirstfix = dec(sbd_bin[151:158])*2
    snr = dec(sbd_bin[158:162])*4
    sp5 = dec(sbd_bin[162:168])


    temp_hex = sbd_hex[6:8]

    #Format ID - should be 40 or else a problem
    formatID = sbd_hex[0:2]
    
    if int(formatID, 16) != 40:
        print 'problemo'

    year_hex = sbd_hex[2:4]
    year_bin = bin(int(year_hex, 16))
    year_dec = int(year_bin[2:6], 2)+2000


    #Output the information to a cvs file using csv writer

    global csvPath

    #The path where the csvPath are found, set in pathsy.py
    datadir =  csvPath
    
    
    os.chdir(os.path.join(datadir))

    #Output file is the last 6 digits of the IMEI and the year
    outputFileName = IMEINumber[-6:] + '_' + str(yyyy) + ".csv"

    
    #Check if file already exists, create it if it does not exist, append to it if it does
    if os.path.exists(outputFileName):
        outputFile = open(outputFileName, "ab")
        c = csv.writer(outputFile)
    else:
        outputFile = open(outputFileName, "wb")
        c = csv.writer(outputFile)
        c.writerow(["IMEI","Year","Month","Day", "Hour", "Minute","Latitude","Longitude","Temperature","Voltage Battery","AtmPress","FormatID"])

       
    #ATMPress defaulted to -99
    ATMPress = -99
    
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM, lat, lon, temp, batt, ATMPress, formatID])

    outputFile.close()



def dec(b):
    return int(b,2)

def bin1(s):
    return str(s) if s<=1 else bin(s>>1) + str(s&1)


