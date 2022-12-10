#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 14:37:32 2022

@author: adam
"""

def process_cryologger(filename, raw_data):

    """
    # Notes: 
        - Cryologger beacon data should be separated into separate CSV files.

    Raw data columns (case sensitive):
    ----------------------------------
    imei
    momsn 
    transmit_time 
    iridium_latitude 
    iridium_longitude 
    iridium_cep 
    data 
    unixtime 
    temperature 
    pressure 
    pitch 
    roll 
    heading 
    latitude 
    longitude 
    satellites 
    hdop 
    voltage 
    transmitDuration 
    iterationCounter or messageCounter
    """
    # Debug message
    log.info("Processing Cryologger")
      
    # Create an empty data frame filled with NAs of length equal to the CSV
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)
      
    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem
    
    # Beacon type
    df["beacon_type"] = "cryologger"
    
    # 3) Unixtime (UTC)
    if "unixtime" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["unixtime"], unit='s')  
    
    # 4) Data transmission timestamp (UTC)
    if"transmit_time" in raw_data:
      df["datetime_transmit"] = pd.to_datetime(raw_data["transmit_time"]

    # 5) Latitude
    if "latitude" in raw_data:
        df["latitude"] = raw_data["latitude"]

    # 6) Longitude
    if "longitude" in raw_data:
        df["longitude"] = raw_data["longitude"]
    
    # 7) Battery voltage
    if "voltage" in raw_data:
        df["vbat"] = raw_data["voltage"]
    
    # 9) Internal temperature
    if("temperature" in raw_data:
       df["ti"] = raw_data["temperature"]
    
    # 11) Barometric pressure
    if("pressure" in raw_data:
       df["bp"] = raw_data["pressure"] * 10 # Conver to hPa
    
    # 12) Pitch
    if("pitch" in raw_data:
       df["pitch"] = raw_data["pitch"]
    
    # 13) Roll
    if("roll" in raw_data:
       df["roll"] = raw_data["roll"]
    
    # 14) Tilt-compensated heading
    if("heading" in raw_data:
       df["heading"] = raw_data["heading"]
    
    # 15) GNSS Satellites
    if("satellites" in raw_data:
       df["satellites"] = raw_data["satellites"]
      
    return(df)