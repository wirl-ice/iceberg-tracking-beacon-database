#! /usr/bin/env python
  # -*- coding: utf-8 -*-
"""
Created on Wed Feb 01 10:08:34 2012
Modified April 2, 2017 by Jill Rajewicz

@author: Cindy Lopes
"""

import os
import glob
from datetime import datetime

from sbd_decoder import decode
import email, getpass, imaplib
import shutil
import logging
import argparse

from paths import archivePath, logPath, tempPath, csvPath

global tempPath

def decodeFolder():
    '''
    Function goes through the temporary folder, tries to decode all of the sbd files
    and if they are decoded moves them to the archive directory
    
    '''
    
    global tempPath    
    
    sbdfiles = glob.glob( os.path.join(tempPath,'*.sbd'))
    sbdfiles.sort()
    #Goes through the files in the temporary folder
    for infile in sbdfiles:
        fileName = infile[infile.rfind("\\")+1:]
    
        #Call the sbdDecoder    
        decodeSuccess = decode(fileName)
        
        if(decodeSuccess):
            
            src = os.path.join(tempPath,fileName)
            
            #This is where you want to put the archived decoded *.sbd set in pathy.py
            dst = archivePath
            
            shutil.move(src, dst)
        else:
            logger.info("In decoding downloaded " + fileName + " was not decoded or transfered to archive")
        
        

def downloadEmails(user=None,pwd=None):
    '''
    Function takes a gmail username and password.
    Goes through emails according to search criteria and downloads the attachment from the selected emails
    
    See http://www.example-code.com/csharp/imap-search-critera.asp for more search examples
    '''  
    
    
    global tempPath
    
    '''-----Enter Username and Password for Gmail Here---'''
    # NOTE less secure app access needs to be 'on'
    #user #enter username user@gmail.com and password between quotations
    #pwd = ""
    
    if user == None:
        user = input("Enter your GMail username:")
    if pwd == None:    
        pwd = getpass.getpass("Enter your password: ")
   
        
    # connecting to the gmail imap server
    m = imaplib.IMAP4_SSL("imap.gmail.com")
    
    m.login(user,pwd)
    
    #m.select("[Gmail]/All Mail") # here you a can choose a mail box like INBOX instead
    m.select("INBOX")
    # use m.list() to get all the mailboxes
    
    
    '''------ Here you can alter the settings to filter which emails should be opened ------'''
    
    '''This seach will open attachments for ALL the emails'''
   # resp, items = m.search(None, "(ALL SUBJECT "")") # you could filter using the IMAP rules here (check http://www.example-code.com/csharp/imap-search-critera.asp)
    
    '''This seach will open attachments for the UNOPENED emails-----------------------Change this back'''
    #resp, items = m.search(None, "UNSEEN")
    #resp, items = m.search(None, "SEEN")
    
    #TODO -get this to work as an argument since a given date:  https://stackoverflow.com/questions/52054196/python-imaplib-search-email-with-date-and-time#52066820
    resp, items = m.search(None, "SINCE 4-Aug-2021")
    
    items = items[0].split() # getting the mails id
    
    print ("Looking through " + str(len(items)) + " emails")
    
    #Keep track of the number of emails that were searched
    searched = str(len(items))
    
    #Keep track of the number of downloads that had attachments, repeated attachments, and sucessfully downloaded
    attached = 0    
    repeated = 0
    downloaded = 0
    

    for emailid in items:
        resp, data = m.fetch(emailid, "(RFC822)") # fetching the mail, "`(RFC822)`" means "get the whole stuff", but you can ask for headers only, etc
        email_body = data[0][1] # getting the mail content
        #mail = email.message_from_string(email_body) # parsing the mail content to get a mail object
        mail = email.message_from_bytes(email_body) # parsing the mail content to get a mail object
        #Check if any attachments at all
        if mail.get_content_maintype() != 'multipart':
            continue
        
        
    
        #print "["+mail["From"]+"] :" + mail["Subject"]
    
        # we use walk to create a generator so we can iterate on the parts and forget about the recursive headache
        for part in mail.walk():
            # multipart are just containers, so we skip them
            if part.get_content_maintype() == 'multipart':
                continue
    
            # is this part an attachment ?
            if part.get('Content-Disposition') is None:
                continue
           
            #If attachment then proceed.... 
            if part.get('Content-Disposition')[0:10] == "attachment":
                         
                         
                #increment the number of attachment emails
                attached +=1
        
                #Extract the year and month from the email was sent
                allDate = mail["Date"]
                dt = datetime.strptime(allDate, "%a, %d %b %Y %H:%M:%S %Z")            
                
                #Add email timestamp to the end of the file
                filename = part.get_filename()
    
                dateFileName = filename[:-4]
                dateFileName = dateFileName + "_" + dt.isoformat()
                dateFileName = dateFileName + filename[-4:]
        
                filename = dateFileName
                counter = 1
    
                # if there is no filename, we create one with a counter to avoid duplicates
                if not filename:
                    filename = 'part-%03d%s' % (counter, 'bin')
                    counter += 1
        
                att_path = os.path.join(tempPath, filename)
        
                #Check if its already there
                if not os.path.isfile(att_path) :
        
                    # finally write the stuff
                    fp = open(att_path, 'wb')
                    fp.write(part.get_payload(decode=True))
                    fp.close()
                    #increment the number of sucessfully downloaded
                    downloaded +=1                    
                    
                else :
                    logger.info("When downloading files from Gmail " + filename + " was downloaded twice and only saved one")
                    
                    #increment the number of repeated downloads
                    repeated += 1
                    
        
    #Write the results of the download to the logger
    logger.info("Once completed downloading files from gmail:  ")
    logger.info(str(searched) + " emails were searched")
    logger.info(str(attached) + " emails had attachments")
    logger.info(str(repeated) + " emails had repeated attachments and were not downloaded")
    logger.info(str(downloaded) + " attachments were downloaded")
    
       

'''For logger'''
logger = logging.getLogger('coverageConvert')
hdlr = logging.FileHandler(os.path.join(logPath, 'Download_and_Decode.log'))
formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
hdlr.setFormatter(formatter)
logger.addHandler(hdlr)
logger.setLevel(logging.INFO)                        

prog_description = '''Download and Decode SBD messages from gmail
  all folders are defined in paths.py
  enter username and password or be prompted
'''    
parser = argparse.ArgumentParser(description = prog_description)
parser.add_argument("-e", "--email", help="downlod emails", action="store_true")
parser.add_argument("-d", "--decode", help="decode sbd files", action="store_true")
parser.add_argument("-u", "--user", help="input gmail email address")
parser.add_argument("-p", "--pwd", help="input gmail email password")
args = parser.parse_args()

import pdb; pdb.set_trace()       

if args.email: 
    downloadEmails(args.user, args.pwd)
if args.decode: 
    decodeFolder()
