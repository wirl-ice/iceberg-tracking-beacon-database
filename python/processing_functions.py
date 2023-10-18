#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 14:37:32 2022

@author: adam
"""

import pandas as pd
import numpy as np
from pathlib import Path

# -----------------------------------------------------------------------------
# Standardized column names
# -----------------------------------------------------------------------------

columns = [
    "beacon_id",
    "beacon_type",
    "datetime_data",
    "datetime_transmit",
    "latitude",
    "longitude",
    "temperature_air",
    "temperature_internal",
    "temperature_surface",
    "pressure",
    "pitch",
    "roll",
    "heading",
    "satellites",
    "voltage",
    "loc_accuracy",
    "distance",
    "speed",

]

# -----------------------------------------------------------------------------
# Standardized sensor value min/max
# -----------------------------------------------------------------------------

# Latitude
latitude_max = 90.0
latitude_min = 40.0

# Longitude
longitude_max = -30.0
longitude_min = -180.0

# Air temperature
temperature_air_max = 40.0
temperature_air_min = -50.0

# Internal temperature
temperature_internal_max = 40.0
temperature_internal_min = -50.0

# Surface temperature
temperature_surface_max = 40.0
temperature_surface_min = -50.0

# Pressure
pressure_max = 1200.0
pressure_min = 800.0

# Pitch
pitch_max = 180.0
pitch_min = -180.0

# Roll
roll_max = 180.0
roll_min = -180.0

# Heading
heading_max = 360.0
heading_min = 0.0

# Satellites
satellites_max = 25
satellites_min = 0

# Battery voltage
voltage_max = 25.0
voltage_min = 0.0

# Location accuracy (Argos)
loc_accuracy_max = 3
loc_accuracy_min = 0


# -----------------------------------------------------------------------------
# Standardization functions
# -----------------------------------------------------------------------------


def process_bio(filename, raw_data):
    """
    Function to convert raw data from a GPS/Iridium beacon developed at the
    Bedford Institute ofOceanography and used by the Department of Fisheries
    and Oceans to a standardized csv before further processing to quality
    added files.

    Raw data columns (case sensitive):
    ----------------------------------
    BID
    VOLTAGE
    GPS_DATE (08-09-15 13:00:00.000000000 or 2011-03-20 13:00:00.0)

    LATITUDE
    LONGITUDE

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "bio"

    # Data transmission timestamp
    if "GPS_DATE" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data["GPS_DATE"], format="%Y-%m-%d %H:%M:%S.%f", errors="coerce"
        ).fillna(
            pd.to_datetime(
                raw_data["GPS_DATE"], format="%y-%m-%d %H:%M:%S.%f", errors="coerce"
            )
        )

    # Latitude
    if "LATITUDE" in raw_data:
        df["latitude"] = raw_data["LATITUDE"]

    # Longitude
    if "LONGITUDE" in raw_data:
        df["longitude"] = raw_data["LONGITUDE"]

    # Battery voltage
    if "VOLTAGE" in raw_data:
        df["voltage"] = raw_data["VOLTAGE"]

    return df


def process_calib_argos(filename, raw_data):
    """
    Function to convert raw data from MetOcean CALIB ARGOS beacons.
    Values for sensor ranges are from CALIB Technical Manual.

    Raw data columns (case sensitive):
    ----------------------------------
    TransmitterID
    WMO
    Latitude
    Longitude
    LocAccuracy
    SatelliteTime
    BeaconTime
    MessageIndex
    AtmPressure
    BattVoltage
    AirTemperature

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Manufacturer sensor range values
    global ta_max, ta_min, bp_max, bp_min, vbat_max, vbat_min
    ta_max = 26.5
    ta_min = -50.0
    bp_max = 1074.6
    bp_min = 920.0
    vbat_max = 18.6
    vbat_min = 6.0

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "calib_argos"

    # Data timestamp
    if "BeaconTime" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["BeaconTime"])

    # 4) Data transmission timestamp
    if "SatelliteTime" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["SatelliteTime"])

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    # Air temperature
    if "AirTemperature" in raw_data:
        df["temperature_air"] = raw_data["AirTemperature"]

    # Barometric pressure
    if "AtmPressure" in raw_data:
        df["pressure"] = raw_data["AtmPressure"]

    # Battery voltage
    if "BattVoltage" in raw_data:
        df["voltage"] = raw_data["BattVoltage"]

    # Battery voltage
    if "LocAccuracy" in raw_data:
        df["loc_accuracy"] = raw_data["LocAccuracy"]

    return df


def process_calib_iridium(filename, raw_data):
    """


    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """
    # Manufacturer sensor range values
    global ta_max, ta_min, bp_max, bp_min, vbat_max, vbat_min
    ta_max = 26.5
    ta_min = -50.0
    bp_max = 1074.6
    bp_min = 920.0
    vbat_max = 18.6
    vbat_min = 6.0

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "calib_iridium"

    # Data timestamp
    if "DATA DATE (UTC)" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["DATA DATE (UTC)"])
    elif "Data Date (UTC)" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["Data Date (UTC)"])

    # 4) Data transmission timestamp
    if "RECEIVED DATE (UTC)" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["RECEIVED DATE (UTC)"])
    elif "Received Date (UTC)" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["Received Date (UTC)"])

    # Latitude
    if "LATITUDE" in raw_data:
        df["latitude"] = raw_data["LATITUDE"]

    # Longitude
    if "LONGITUDE" in raw_data:
        df["longitude"] = raw_data["LONGITUDE"]

    # Surface temperature
    if "SST" in raw_data:
        df["temperature_surface"] = raw_data["SST"]

    # Barometric pressure
    if "BP" in raw_data:
        df["pressure"] = raw_data["BP"]

    # Battery voltage
    if "VBAT" in raw_data:
        df["voltage"] = raw_data["VBAT"]

    return df


def process_canatec(filename, raw_data):
    """
    Function to convert raw Canatec beacon data to a standardized CSV
    before further processing to 'quality added' files.
    Sensor range values are from Canatec Ice Drift Beacon model V3.1-THA-12
    Operation Manual

    Raw data columns (case sensitive):
    -----------------------------------
    ReadingDate
    Latitude
    Longitude
    Elevation
    Heading
    Speed
    Fix
    Satellites
    HDOP
    VDOP
    VerticalVelocity
    Pressure
    TempExternal
    TempInternal
    BeaconAlarmState
    BatteryVoltage
    ModemVoltage
    WindSpeed
    WindDirection

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Unique beacon ID
    beacon_id = Path(filename).stem

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = beacon_id

    # Beacon type
    df["beacon_type"] = "canatec"

    # Data timestamp
    # Can be either: ymd_hms, ymd_hm or mdy_hms, mdy_hm.
    # Code will check for appropriate format and can handle missing seconds.

    format_1 = "%m/%d/%Y %I:%M:%S %p"
    format_2 = "%Y-%m-%d %H:%M:%S"
    format_3 = "%m/%d/%Y %H:%M"
    
    if "ReadingDate" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data["ReadingDate"]) # , infer_datetime_format=True is now deprecated
        """
        df["datetime_data"] = pd.to_datetime(
            raw_data["ReadingDate"], format=format_1, errors="coerce"
        ).fillna(
            pd.to_datetime(raw_data["ReadingDate"], format=format_2, errors="coerce")
        )
        """

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    # Air temperature
    if "TempExternal" in raw_data:
        df["temperature_air"] = raw_data["TempExternal"]

    # Internal temperature
    if "TempInternal" in raw_data:
        df["temperature_internal"] = raw_data["TempInternal"]

    # Barometric pressure
    if "Pressure" in raw_data:
        df["pressure"] = raw_data["Pressure"]

    # Battery voltage
    if "BatteryVoltage" in raw_data:
        df["voltage"] = raw_data["BatteryVoltage"]

    # Satellites
    if "Satellites" in raw_data:
        df["satellites"] = raw_data["Satellites"]

    return(df)

def process_iceb(filename, raw_data):
    """
    Function to convert raw MetOcean Ice Beacon  data to a standardized CSV
    before further processing to 'quality added' files.
    
    Sensor range values are from 
    Operation Manual

    Raw data columns (case sensitive):
    -----------------------------------
    ReadingDate
    Latitude
    Longitude
    Elevation
    Heading
    Speed
    Fix
    Satellites
    HDOP
    VDOP
    VerticalVelocity
    Pressure
    TempExternal
    TempInternal
    BeaconAlarmState
    BatteryVoltage
    ModemVoltage

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Unique beacon ID
    beacon_id = Path(filename).stem

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = beacon_id

    # Beacon type
    df["beacon_type"] = "canatec"

    # Data timestamp
    if "ReadingDate" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data["ReadingDate"], "%m/%d/%Y %I:%M:%S %p") # 7/16/2009 6:15:00 PM

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    # Air temperature
    if "TempExternal" in raw_data:
        df["temperature_air"] = raw_data["TempExternal"]

    # Internal temperature
    if "TempInternal" in raw_data:
        df["temperature_internal"] = raw_data["TempInternal"]

    # Barometric pressure
    if "Pressure" in raw_data:
        df["pressure"] = raw_data["Pressure"]

    # Battery voltage
    if "BatteryVoltage" in raw_data:
        df["voltage"] = raw_data["BatteryVoltage"]

    # Satellites
    if "Satellites" in raw_data:
        df["satellites"] = raw_data["Satellites"]

    return(df)

def process_ccg(filename, raw_data):
    """
    Function to convert raw data (CSV) from a self-location datum marking
    buoy deployed by the CCGS in 2011 (beacon make/model unknown) to a
    standardized CSV before further processing to quality added files.

    Raw data columns (case sensitive):
    ----------------------------------
    Buoy Name
    Time (2011 Jun 18 18:20:02 UTC)
    Latitude
    Longitude
    Temperature
    Drogue Depth (m)

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "ccg"

    if "Time" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data["Time"], format="%Y %b %d %H:%M:%S UTC"
        )

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    return df


def process_cryologger(filename, raw_data):
    """
    Function to convert raw CSV data from Cryologger ice tracking beacons to
    standardized CSV before further processing to 'quality added' files.

    Raw data columns (case sensitive):
    imei
    momsn
    transmit_time
    iridium_latitude
    iridium_longitude
    iridium_cep
    data
    unixtime
    temperature_int
    humidity_int
    pressure_int
    pitch
    roll
    heading
    latitude
    longitude
    satellites
    hdop
    voltage
    transmitDuration
    messageCounter

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "cryologger"

    # 3) Unixtime (UTC)
    if "unixtime" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["unixtime"], unit="s")

    # 4) Data transmission timestamp (UTC)
    if "transmit_time" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["transmit_time"])

    # 5) Latitude
    if "latitude" in raw_data:
        df["latitude"] = raw_data["latitude"]

    # 6) Longitude
    if "longitude" in raw_data:
        df["longitude"] = raw_data["longitude"]

    # 7) Battery voltage
    if "voltage" in raw_data:
        df["voltage"] = raw_data["voltage"]

    # 9) Internal temperature
    if "temperature_int" in raw_data:
        df["temperature_internal"] = raw_data["temperature_int"]

    # 11) Barometric pressure
    if "pressure_int" in raw_data:
        df["pressure"] = raw_data["pressure_int"] * 10  # Convert to hPa

    # 12) Pitch
    if "pitch" in raw_data:
        df["pitch"] = raw_data["pitch"]

    # 13) Roll
    if "roll" in raw_data:
        df["roll"] = raw_data["roll"]

    # 14) Tilt-compensated heading
    if "heading" in raw_data:
        df["heading"] = raw_data["heading"]

    # 15) GNSS Satellites
    if "satellites" in raw_data:
        df["satellites"] = raw_data["satellites"]

    return df


def process_iabp(filename, raw_data):
    """

    Function to convert raw data downloaded from the International Arctic
    Buoy Program website to a standardized csv before further processing to
    'quality added' files.

    Note: Beacon type should be checked to deal with format of data
    from IABP repository as many different beacon types report there.

    Raw data columns (case sensitive):
    ---------------------------------
    BuoyID
    Date
    Tear
    Hour
    Min
    DOY
    POS_DOY
    Lat
    Lon
    BP
    Ta
    Ts

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "iabp"

    if "DOY" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["Year"], format="%Y") \
                     + raw_data["DOY"].sub(1).apply(pd.Timedelta, unit='D')

    # Latitude
    if "Lat" in raw_data:
        df["latitude"] = raw_data["Lat"]

    # Longitude
    if "Lon" in raw_data:
        df["longitude"] = raw_data["Lon"]

    # Air temperature
    if "Ta" in raw_data:
        df["temperature_air"] = raw_data["Ta"]
        
    # Surface temperature
    if "Ts" in raw_data:
        df["temperature_surface"] = raw_data["Ts"]

    # Pressure
    if "BP" in raw_data:
        df["pressure"] = raw_data["BP"]
        
    return df


def process_iip(filename, raw_data):
    """
    Function to convert raw CSV data from IIP ice tracking beacons to
    standardized CSV before further processing to 'quality added' files.

    Raw data columns (case sensitive):
    ----------------------------------
    INDEX
    ID
    DATETIME (2019-04-29T23:03UTC)
    LATITUDE
    LONGITUDE

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Manufacturer sensor range values
    # --------------------------------
    global vbat_max
    vbat_max = 8.0

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "iip"

    if "DATETIME" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data["DATETIME"], format="%Y-%m-%dT%H:%MUTC"
        )

    # Latitude
    if "LATITUDE" in raw_data:
        df["latitude"] = raw_data["LATITUDE"]

    # Longitude
    if "LONGITUDE" in raw_data:
        df["longitude"] = raw_data["LONGITUDE"]

    return df


def process_navidatum(filename, raw_data):
    """
    Function to convert a csv of raw data from a Navidatum tracking beacon to a standardized csv before
    further processing to 'quality added' files.

    Raw data columns (case sensitive):
    -----------------------------------
    Date (02/08/2012 21:01)
    Latitude
    Longitude

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "navidatum"

    if "Date" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["Date"], format="%d/%m/%Y %H:%M")

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    return df


def process_oceanetic(filename, raw_data):
    """

    Raw data columns (case sensitive):
    ----------------------------------
    IMEI
    Year
    Month
    Day
    Hour
    Minute
    Latitude
    Longitude
    Temperature
    Voltage Battery
    AtmPress
    FormatID

    *or*

    beacon id
    yr
    mm
    dd
    hr
    lat
    long

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "oceanetic"

    # Datetime format for beacon IDs: 2011_300034013463170 and 0082470
    if "Year" in raw_data:
        df["datetime_data"] = pd.to_datetime(
            raw_data[["Year", "Month", "Day", "Hour", "Minute"]]
        )
    # Datetime format for all other beacon IDs
    elif "yr" in raw_data:
        raw_data.rename(
            columns={"yr": "year", "mm": "month", "dd": "day", "hr": "hour"},
            inplace=True,
        )
        df["datetime_data"] = pd.to_datetime(raw_data[["year", "month", "day", "hour"]])

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]
    elif "lat" in raw_data:
        df["latitude"] = raw_data["lat"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]
    elif "long" in raw_data:
        df["longitude"] = raw_data["long"]

    # Longitude
    if "Temperature" in raw_data:
        df["temperature_internal"] = raw_data["Temperature"]

    # Pressure
    if "AtmPress" in raw_data:
        df["pressure"] = raw_data["AtmPress"]

    # Longitude
    if "Voltage Battery" in raw_data:
        df["voltage"] = raw_data["Voltage Battery"]

    return df


def process_rockstar(filename, raw_data):
    """

    Function to convert a CSV of raw data from a RockSTAR Iridium GPS beacon
    to a standardized CSV before further processing to 'quality added' files.

    Raw data columns (case sensitive):
    ----------------------------------
    ID
    GPS Time (UTC) (17-08-2016 20:00:06)
    Latitude
    Longitude

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "rockstar"

    # Datetime format for beacon IDs: 2011_300034013463170 and 0082470
    if "GPS Time (UTC)" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["GPS Time (UTC)"], dayfirst=True)

    # Latitude
    if "Latitude" in raw_data:
        df["latitude"] = raw_data["Latitude"]

    # Longitude
    if "Longitude" in raw_data:
        df["longitude"] = raw_data["Longitude"]

    return df


def process_solara(filename, raw_data):
    """
    Function to convert beacon data collected by Solara iceberg trackers
    into a standardized CSV before further processing to 'quality added' files.

    Raw data columns (case sensitive):
    ----------------------------------
    serial
    alias
    lat
    long
    timestamp

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Unique beacon ID
    beacon_id = Path(filename).stem

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = beacon_id

    # Beacon type
    df["beacon_type"] = "solara"

    #format_1 = "%Y-%m-%d %H:%M:%S"
    #format_2 = "%d/%m/%Y %H:%M:%S"

    if "timestamp" in raw_data:

            df["datetime_data"] = pd.to_datetime(
                raw_data["timestamp"])

    # Latitude
    if "lat" in raw_data:
        df["latitude"] = raw_data["lat"]

    # Longitude
    if "long" in raw_data:
        df["longitude"] = raw_data["long"]

    return df


def process_svp(filename, raw_data):
    """

    Notes:
    - SVP-I-BXGS-LP beacons have BP, GPS, and SST
    - SVP-I-XXGS-LP beacons have GPS and SST, no BP
    - SVP-I-BXGSA-L-AD beacons have BP, GPS, SST, AT, lithium battery and are
    designed for air deployment in Arctic regions
    - Sensor range values are from iSVP technical manual v1.5 and SVP-I-BXGSA-L-AD
    Deployment and User's Manual Version 1.0

    Raw data columns (case sensitive):
    ----------------------------------
    Asset Name / Asset.Name
    Asset Id  / Asset.Id
    Data Date (UTC) / DataDate_UTC
    Received Date (UTC)
    LATITUDE
    LONGITUDE
    FMTID
    YEAR
    MONTH
    DAY
    HOUR
    MIN
    SST
    BP
    BPT
    AT
    VBAT
    GPSDELAY
    SNR
    TTFF
    SBDTIME
    Report Body / Report.Body

    Parameters
    ----------
    filename : string
        Path to deployment CSV file.
    raw_data : Pandas dataframe
        Pandas dataframe of beacon deployment CSV file.

    Returns
    -------
    Standardized Pandas dataframe ready for cleaning.

    """

    # Manufacturer sensor ranges
    # --------------------------------
    global ta_max, ta_min, ts_max, ts_min, bp_max, bp_min, vbat_max, vbat_min
    ta_max = 42.3
    ta_min = -60.0
    ts_max = 42.3
    ts_min = -60
    bp_max = 1054.7
    bp_min = 850
    vbat_max = 17.6
    vbat_min = 5.0

    # Create an empty data frame of length equal to the CSV that is filled with NAs
    df = pd.DataFrame(np.nan, index=np.arange(len(raw_data)), columns=columns)

    # Unique beacon identifier
    df["beacon_id"] = Path(filename).stem

    # Beacon type
    df["beacon_type"] = "svp"

    # Data timestamp
    if "DATA DATE (UTC)" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["DATA DATE (UTC)"])
    elif "Data Date (UTC)" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["Data Date (UTC)"])
    elif "DataDate_UTC" in raw_data:
        df["datetime_data"] = pd.to_datetime(raw_data["DataDate_UTC"])

    # Data transmission timestamp
    if "RECEIVED DATE (UTC)" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["RECEIVED DATE (UTC)"])
    elif "Received Date (UTC)" in raw_data:
        df["datetime_transmit"] = pd.to_datetime(raw_data["Received Date (UTC)"])

    # Latitude
    if "LATITUDE" in raw_data:
        df["latitude"] = raw_data["LATITUDE"]

    # Longitude
    if "LONGITUDE" in raw_data:
        df["longitude"] = raw_data["LONGITUDE"]

    # Air temperature
    if "AT" in raw_data:
        df["temperature_air"] = raw_data["AT"]

    # Surface temperature
    if "SST" in raw_data:
        df["temperature_surface"] = raw_data["SST"]

    # Barometric pressure
    if "BP" in raw_data:
        df["pressure"] = raw_data["BP"]

    # Battery voltage
    if "VBAT" in raw_data:
        df["voltage"] = raw_data["VBAT"]

    return df


def get_function(beacon_id):
    """
    Parameters
    ----------
    beacon_id : str
        Unique beacon identifier.

    Returns
    -------
    Appropriate function used to process the deployment CSV file.

    """

    logger.info("Executing: get_function()")

    function_dict = {
        "1997_12995": process_calib_argos,
        "2000_11254": process_calib_argos,
        "2002_1104": process_calib_argos,
        "2003_11247": process_calib_argos,
        "2003_11255": process_calib_argos,
        "2004_16795": process_calib_argos,
        "2007_11251": process_calib_argos,
        "2008_11255": process_calib_argos,
        "2008_16794": process_calib_argos,
        "2008_300034012610510": process_bio,
        "2009_11257": process_calib_argos,
        "2009_12996": process_calib_argos,
        "2009_16792": process_calib_argos,
        "2009_16795": process_calib_argos,
        "2009_26973": process_canatec,
        "2009_300034012519990": process_bio,
        "2009_300034012571050": process_canatec, #iceb
        "2009_300034012616000": process_bio,
        "2010_11256": process_calib_argos,
        "2010_12993": process_calib_argos,
        "2010_12994": process_calib_argos,
        "2010_300034012592660": process_canatec,
        "2010_93693": process_calib_argos,
        "2011_11247": process_calib_argos,
        "2011_12995": process_calib_argos,
        "2011_12997": process_calib_argos,
        "2011_16793": process_calib_argos,
        "2011_300034012484860": process_bio,
        "2011_300034012489850": process_bio,
        "2011_300034013149880": process_ccg,
        "2011_300034013439530": process_ccg,
        "2011_300034013453190": process_ccg,
        "2011_300034013453240": process_ccg,
        "2011_300034013458130": process_ccg,
        "2011_300034013463170": process_oceanetic,
        "2011_300234010031950": process_svp,
        "2011_300234010033940": process_svp,
        "2011_300234010035940_PII-A": process_svp,
        "2011_300234010035940_PII-B": process_svp,
        "2011_300234010955690": process_svp,
        "2011_300234010955700": process_svp,
        "2011_300234010958690_PII-A": process_svp,
        "2011_300234010958690_PII-B": process_svp,
        "2011_300234010959690": process_svp,
        "2012_100000000000000": process_navidatum,
        "2012_11248": process_calib_argos,
        "2012_11249": process_calib_argos,
        "2012_11251": process_calib_argos,
        "2012_11252": process_calib_argos,
        "2012_300034012480860": process_bio,
        "2012_300034012480920": process_bio,
        "2012_300234010082470": process_oceanetic,
        "2012_300234010132070": process_svp,
        "2013_300034013460170": process_oceanetic,
        "2013_300034013461170": process_oceanetic,
        "2013_300034013464160": process_oceanetic,
        "2013_300034013464170": process_oceanetic,
        "2013_300034013468160": process_oceanetic,
        "2013_300234011240410": process_canatec,
        "2013_300234011241410": process_canatec,
        "2013_300234011242410": process_canatec,
        "2013_300234011938510": process_canatec,
        "2014_300234060544160": process_svp,
        "2014_300234061763040": process_calib_iridium,
        "2015_300134010204980": process_solara,
        "2015_300134010505190": process_solara,
        "2015_300134010906790": process_solara,
        "2015_300134010907780": process_solara,
        "2015_300234060104820": process_svp,
        "2015_300234060435010": process_svp,
        "2015_300234061762030": process_calib_iridium,
        "2015_300234062790480": process_canatec,
        "2015_300234062791420": process_canatec,
        "2015_300234062791700": process_canatec,
        "2015_300234062792490": process_canatec,
        "2015_300234062792500": process_canatec,
        "2015_300234062794470": process_canatec,
        "2015_300234062795460": process_canatec,
        "2015_300234062795640": process_canatec,
        "2015_300234062796490": process_canatec,
        "2015_300234062796640": process_canatec,
        "2016_300234060172440": process_rockstar,
        "2016_300234060172670": process_rockstar,
        "2016_300234061761040": process_calib_iridium,
        "2016_300234061763030": process_calib_iridium,
        "2016_300234061768060": process_calib_iridium,
        "2016_300234062950220": process_iabp,
        "2016_300234062951220": process_iabp,
        "2016_300234062957250": process_iabp,
        "2016_300234063513450": process_calib_iridium,
        "2016_300234063515450": process_calib_iridium,
        "2016_300234064706730": process_rockstar,
        "2016_300234064808170": process_rockstar,
        "2016_300234064808210": process_rockstar,
        "2016_300434063218800": process_rockstar,
        "2016_300434063417140": process_rockstar,
        "2017_300234010025000": process_rockstar,
        "2017_300234060169280": process_rockstar,
        "2017_300234060177480": process_rockstar,
        "2017_300234060270020": process_rockstar,
        "2017_300234060272000": process_rockstar,
        "2017_300234060276010": process_rockstar,
        "2017_300234060563100": process_rockstar,
        "2017_300234060692710": process_rockstar,
        "2017_300234060699700": process_rockstar,
        "2017_300234062324750": process_svp,
        "2017_300234062325760": process_svp,
        "2017_300234062327750": process_svp,
        "2017_300234062328750": process_svp,
        "2017_300234063516450": process_calib_iridium,
        "2018_300234060362670": process_rockstar,
        "2018_300234060367670": process_rockstar,
        "2018_300234062807520": process_rockstar,
        "2018_300234066241900": process_solara,
        "2018_300234066549270": process_solara,
        "2018_300234066545280": process_solara,
        "2018_300234066443790": process_solara,
        "2018_300234066549280": process_solara,
        
        "2018_300434063411050": process_cryologger,
        "2018_300434063415110": process_cryologger,
        "2018_300434063415160": process_cryologger,
        "2018_300434063416060": process_cryologger,
        "2018_300434063418130": process_cryologger,
        "2018_300434063419120": process_cryologger,
        "2019_1012-2669855": process_iip,
        "2019_1027-2670078": process_iip,
        "2019_1124-2670293": process_iip,
        "2019_2001-2632176": process_iip,
        "2019_2009-2632235": process_iip,
        "2019_2011-2632168": process_iip,
        "2019_2013-2670502": process_iip,
        "2019_2015-2671136": process_iip,
        "2019_2020-2671162": process_iip,
        "2019_2022-2670073": process_iip,
        "2019_2030-2670510": process_iip,
        "2019_2040-2670142": process_iip,
        "2019_2045-2670074": process_iip,
        "2019_300234060206850": process_rockstar,
        "2019_300234060370740": process_rockstar,
        "2019_300234060698700": process_rockstar,
        "2019_300234063265700": process_cryologger,
        "2019_300234065254740": process_cryologger,
        "2019_300434063392070": process_cryologger,
        "2019_300434063394110": process_cryologger,
        "2019_300434063494100": process_cryologger,
        "2019_300434063495310": process_cryologger,
        "2019_3037-2674613": process_iip,
        "2019_4013-2674911": process_iip,
        "2019_4022-2674941": process_iip,
        "2019_4025-2678597": process_iip,
        "2019_4027-2670071": process_iip,
        "2019_4028-2671160": process_iip,
        "2021_300434065734810": process_cryologger,
        "2021_300434065732760": process_cryologger,
        "2021_300434065868240": process_cryologger,
        "2021_300434065860260": process_cryologger,
        "2021_300434065869240": process_cryologger,
        "2021_300434065860320": process_cryologger,
        "2021_300434065864290": process_cryologger,
        "2021_300434065861350": process_cryologger,
        "2021_300434063291950": process_cryologger,
        "2021_300434063497310": process_cryologger,
        "2021_300234011750690": process_calib_iridium,
        "2021_300234011750710": process_calib_iridium,
        "2021_300234011751700": process_calib_iridium,
        "2021_300234011752700": process_calib_iridium,
        "2021_300234060725890": process_calib_iridium,
        "2023_300434063290950": process_cryologger,

    }
    function = function_dict[beacon_id]

    logger.info("Function selected: {}".format(function.__name__))

    return function
