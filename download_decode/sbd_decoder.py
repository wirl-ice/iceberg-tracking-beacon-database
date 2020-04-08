"""
SBD Decoder

Can be called with a specific filename (make sure the correct directory is set in datadir):
    sbdDecoder.main(fileName)

The filename can also be manually set by in the first if statement by chaning the filename manually.

"""
import os
import binascii

import metOceanDecoder
import oceaneticDecoder



from paths import tempPath


def main(fileName = ""):
    
    global tempPath


    '''-------If you want to decode a particular file and don't give the script a filename upon running change the filename here to your file----'''
    if fileName == "":
        #fileName = ""
        return


    #The directory the sbc are decoded into, set in paths.py
    datadir = tempPath
    

    #Extract the IMEI Number from the file name (ie 300034013463170)
    underScore = os.path.basename(fileName).find('_')
    IMEINumber = os.path.basename(fileName)[:underScore]

    '''
    Extract the year and month of the email from the filename.
    This year value will be used by the oceanetic beacons since no year value is encoded in the sbd file.
    The month from the email will be used in the oceanetic decoder to tweak the year if the message was sent on
    December 31st or January 1st
    '''
    yyyy = fileName[-8:-4]
    mm = fileName[-12:-9]

    
    
    #Open the file and read it
    os.chdir(os.path.join(datadir)) 


    fp = open(fileName,"rb")

    fp.seek(0,2) 
    file_size = fp.tell()
    fp.seek(0,0) 
    sbd_raw = fp.read(file_size)

    fp.close

    sbd_hex = binascii.b2a_hex(sbd_raw)

    sbd_bin = bin(int(sbd_hex, 16))[2:]

    n_pad = file_size*8 - len(sbd_bin)
    sbd_bin = '0'*n_pad + sbd_bin


    #Check the file size and call the appropriate script
    if(file_size == 21):
        metOceanDecoder.main(IMEINumber, sbd_raw)
        return True
    elif(file_size == 31):
        oceaneticDecoder.main(IMEINumber, sbd_raw, yyyy, mm)    #You need to pass the email year and month to the oceanetic decoder
        return True
    else:
        print "Problem: File size does match any known size"
        print file_size
        return False

if __name__ == '__main__':
    main("")
    


