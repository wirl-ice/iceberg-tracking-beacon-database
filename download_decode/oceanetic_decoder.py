'''
Updated: January 13th, 2011

Script takes an IMEI Number, year, month, and data from a cooresponding *.sbd file for an oceanetic beacon.

It extracts the information form the data, creates or appends to a *.csv file with the form ######_yyyy.csv (# is the last 6 digits of the IMEI Number)
in the following format:

IMEI	        Year	Month	Day	Hour	Minute	Latitude    Longitude   Temperature  Voltage Battery	AtmPress    FormatID
3.00034E+14	2011	10	14	18	0	74.26435    -143.579	-99	     10	                -99	    0

The script checks if the date of the message was December 31st or January 1st. If so it checks the month of the email and compares it with the month of the
message, correcting the year if needed.

5 Relative positions are taken at 10 minute intervals after the origional position. These are decoded and added to the csv file in their own row.

'''

import binascii
import numpy
import csv
import os
import datetime

from paths import csvPath

def main(IMEINumber, sbd_raw, yyyy, emailMonth):



    file_size = 31
    
    sbd_hex = binascii.b2a_hex(sbd_raw)
    sbd_bin = bin(int(sbd_hex, 16))[2:]
    n_pad = file_size*8 - len(sbd_bin)
    sbd_bin = '0'*n_pad + sbd_bin


    #Splice up the file according to the given format
    msg_id = dec(sbd_bin[0:2])
    batt = dec(sbd_bin[2:7])*1/2+1
    dd = dec(sbd_bin[7:12])
    mm = dec(sbd_bin[12:16])
    HH = dec(sbd_bin[16:21])


    '''-------Check if the year needs to be changed-------'''
    yyyy = checkYear (yyyy,mm,dd,emailMonth)


    #Determine the latitude
    Latitude = finalPosition(sbd_bin[21:28], sbd_bin[28:44], sbd_bin[44:45])

    
    #Determine the longitude
    Longitude = finalPosition (sbd_bin[45:53], sbd_bin[53:69], sbd_bin[69:70])

    
    '''Here the relative position are calculated'''
    #Relative Position 
    Rel_Lat_1 = relativePosition(Latitude,sbd_bin[71:85], sbd_bin[70:71])
    Rel_Lon_1 = relativePosition(Longitude, sbd_bin[86:103], sbd_bin[85:86])
    
    #Relative Position 2
    Rel_Lat_2 = relativePosition(Latitude,sbd_bin[104:118], sbd_bin[103:104])
    Rel_Lon_2 = relativePosition(Longitude, sbd_bin[119:136], sbd_bin[118:119])

    
    #Relative Position 3
    Rel_Lat_3 = relativePosition(Latitude,sbd_bin[137:151], sbd_bin[136:137])
    Rel_Lon_3 = relativePosition(Longitude, sbd_bin[152:169], sbd_bin[151:152])

    
    #Relative Position 4
    Rel_Lat_4 = relativePosition(Latitude,sbd_bin[170:184], sbd_bin[169:170])
    Rel_Lon_4 = relativePosition(Longitude, sbd_bin[185:202], sbd_bin[184:185])


    #Relative Position 5
    Rel_Lat_5 = relativePosition(Latitude,sbd_bin[203:217], sbd_bin[202:203])
    Rel_Lon_5 = relativePosition(Longitude, sbd_bin[218:235], sbd_bin[217:218])

    not_used = dec(sbd_bin[235:240])
    msg_chk = dec(sbd_bin[240:248])

    global csvPath
   
   #The path where the csvPath are found, set in pathsy.py
    datadir =  csvPath
    
    os.chdir(os.path.join(datadir))
    

    #Output file is the last 6 digits of the IMEI and the year
    outputFileName = IMEINumber[-7:] + '_' + str(yyyy) + ".csv"


    #Check if file already exists, create it if it does not exist, append to it if it does
    if os.path.exists(outputFileName):
        outputFile = open(outputFileName, "ab")
        c = csv.writer(outputFile)
    else:
        outputFile = open(outputFileName, "wb")
        c = csv.writer(outputFile)
        c.writerow(["IMEI","Year","Month","Day", "Hour", "Minute","Latitude","Longitude","Temperature","Voltage Battery","AtmPress","FormatID"])

    #Set defaults, AtmPress = -99, minutes = 0, temp = -99, formatID = 0
    ATMPress = -99
    MM = 0
    temp = -99
    formatID = 0
    
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM, Latitude, Longitude, temp, batt, ATMPress, formatID])

    #Now write the 5 relative positions
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM+10, Rel_Lat_1, Rel_Lon_1, temp, batt, ATMPress, formatID])
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM+20, Rel_Lat_2, Rel_Lon_2, temp, batt, ATMPress, formatID])
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM+30, Rel_Lat_3, Rel_Lon_3, temp, batt, ATMPress, formatID])
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM+40, Rel_Lat_4, Rel_Lon_4, temp, batt, ATMPress, formatID])
    c.writerow([IMEINumber, yyyy, mm, dd, HH, MM+50, Rel_Lat_5, Rel_Lon_5, temp, batt, ATMPress, formatID])
    
    

    outputFile.close()



'''
Function takes the data for the degree, minutes, and sign for a position and converts it to a final position
'''
def finalPosition(degree, minutes, sign):
    
    degree = dec(degree)/1.0
    #divide by 1000.0 to create a float (3 decimal places)
    minutes = dec(minutes)/1000.0;
    #Determine if the latitude is North or South
    sign = dec(sign)

    if (sign == 0):
        degree = degree*(-1)
        minutes = minutes*(-1)
        
    #Add the minutes to the position
    finalPosition = degree + minutes/60.0

    return finalPosition

'''
Function takes the original position (latitude or longitude) and the sign and minutes for the relative position and
returns the final relative position
'''
def relativePosition(position, sign, minutes):

    minutes = dec(minutes)/1000.0/60.0

    if (sign == 0):
         minutes = minutes*(-1)

    return position+minutes
         

'''
Function takes the year and month from the email and the month and day from beacon and checks if the transmission date is Dec 31 or January 1st.
By comparing the month from the sbd file and the month of the email it can be determined if the year needs to be altered
'''
def checkYear(yyyy,mm,dd,emailMonth):

    #Check if the month and day (from the .sbd file) is Dec 31 or Jan 1st
    if (mm == 12 and dd == 31) or (mm == 1 and dd == 1):

        #If the month from the email is January and the month from the sbd file is December change the year
        if mm == 12 and emailMonth == "Jan":
            print("Warning email month does not match sbd month changing year")
            return (yyyy-1)

        #If the month from the email is December and the month from the sbd file is January change the year
        if mm == 1 and emailMonth == "Dec":
            print("Warning the email month does not match the sbd month changing year")
            return (yyyy+1)

    return yyyy    

    
def dec(b):
    return int(b,2)

def bin1(s):
    return str(s) if s<=1 else bin(s>>1) + str(s&1)




    
