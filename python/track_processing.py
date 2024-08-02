#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
track_processing.py


"""

import os
import sys
import argparse
import logging
from pathlib import Path
import pandas as pd
import geopandas as gpd
from shapely.geometry import LineString
import numpy as np
import pyproj
import standardization_functions

# import openpyxl


def track_read(raw_file, fn_std="fn_standard"):
    """
    Read the track raw data using the specified standardization function.

    Parameters
    ----------
    raw_file : str
        path to the raw data file.
    fn_std : str, optional
        name of the standardization function to use. The default is "fn_standard".

    Returns
    -------
    sdf : pandas Dataframe
        Standardized beacon track pandas dataframe.

    """
    func = getattr(standardization_functions, fn_std)
    sdf = func(raw_file)

    return sdf


def track_clean(sdf, model, specs_df):
    """
    Assign NaN to sensor values that exceed the minimum/maximum ranges.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.
    model : str
        beacon model name to look up in specs_df
    specs_df : Pandas dataframe
        beacon specifications table with ranges of sensor values.

    Returns
    -------
    sdf : Pandas dataframe
        Standardized track data - now cleaned

    """
    # logger.info("Executing: clean_data()")

    # retrieve the default specs and the one for the beacon model in question
    default_specs = specs_df.loc[specs_df.beacon_model == "Default"]
    beacon_specs = specs_df.loc[specs_df.beacon_model == model]

    assert (
        len(default_specs) == 1
    ), "unkown model, check spelling -or- duplicate model retrieved"
    assert (
        len(beacon_specs) == 1
    ), "unkown model, check spelling -or- duplicate model retrieved"

    # find which specs are undefined (=?) and replace with default
    beacon_specs_ind = beacon_specs.loc[:].values == "?"
    beacon_specs_ind = np.argwhere(beacon_specs_ind.flatten()).tolist()
    beacon_specs_ind = [i for row in beacon_specs_ind for i in row]
    beacon_specs.iloc[0, beacon_specs_ind] = default_specs.iloc[0, beacon_specs_ind]

    # now coerce the data to floats, satellites is really an integer, so try, but skip if NA
    specs = beacon_specs.filter(regex="_max|_min")
    specs = specs.astype(float)
    try:
        specs = specs.astype({"satellites_max": int, "satellites_min": int})
    except:
        pass

    # Latitude
    sdf.loc[
        (sdf["latitude"] >= specs.latitude_max.iloc[0])
        | (sdf["latitude"] <= specs.latitude_min.iloc[0]),
        "latitude",
    ] = np.nan

    # Longitude
    sdf.loc[
        (sdf["longitude"] >= specs.longitude_max.iloc[0])
        | (sdf["longitude"] <= specs.longitude_min.iloc[0])
        | (sdf["longitude"] == 0),
        "longitude",
    ] = np.nan

    # Air temperature
    sdf.loc[
        (sdf["temperature_air"] >= specs.temperature_air_max.iloc[0])
        | (sdf["temperature_air"] <= specs.temperature_air_min.iloc[0]),
        "temperature_air",
    ] = np.nan

    # Internal temperature
    sdf.loc[
        (sdf["temperature_internal"] >= specs.temperature_internal_max.iloc[0])
        | (sdf["temperature_internal"] <= specs.temperature_internal_min.iloc[0]),
        "temperature_internal",
    ] = np.nan

    # Surface temperature
    sdf.loc[
        (sdf["temperature_surface"] >= specs.temperature_surface_max.iloc[0])
        | (sdf["temperature_surface"] <= specs.temperature_surface_min.iloc[0]),
        "temperature_surface",
    ] = np.nan

    # Pressure
    sdf.loc[
        (sdf["pressure"] >= specs.pressure_max.iloc[0])
        | (sdf["pressure"] <= specs.pressure_min.iloc[0]),
        "pressure",
    ] = np.nan

    # Pitch
    sdf.loc[
        (sdf["pitch"] >= specs.pitch_max.iloc[0])
        | (sdf["pitch"] <= specs.pitch_min.iloc[0]),
        "pitch",
    ] = np.nan

    # Roll
    sdf.loc[
        (sdf["roll"] >= specs.roll_max.iloc[0])
        | (sdf["roll"] <= specs.roll_min.iloc[0]),
        "roll",
    ] = np.nan

    # Heading
    sdf.loc[
        (sdf["heading"] >= specs.heading_max.iloc[0])
        | (sdf["heading"] <= specs.heading_min.iloc[0]),
        "heading",
    ] = np.nan

    # Satellites
    sdf.loc[
        (sdf["satellites"] >= specs.satellites_max.iloc[0])
        | (sdf["satellites"] <= specs.satellites_min.iloc[0]),
        "satellites",
    ] = np.nan

    # Battery voltage
    sdf.loc[
        (sdf["voltage"] >= specs.voltage_max.iloc[0])
        | (sdf["voltage"] <= specs.voltage_min.iloc[0]),
        "voltage",
    ] = np.nan

    # Drop all rows where datetime_data, latitude or longitude is nan
    sdf.dropna(subset=["datetime_data", "latitude", "longitude"], inplace=True)

    sdf = sdf.round(
        {
            "temperature_air": 2,
            "temperature_internal": 2,
            "temperature_surface": 2,
            "pressure": 2,
            "pitch": 2,
            "roll": 2,
            "heading": 2,
            "voltage": 2,
        }
    )

    return sdf


def track_order(sdf):
    """
    Order the track and remove redundant entries.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.

    Returns
    -------
    sdf : Pandas dataframe
        Standardized track data - now ordered.

    """
    # sort by datetime_data, and loc_accuracy if available.
    sdf.sort_values(["datetime_data", "loc_accuracy"], inplace=True)
    # look for repeated values
    # sdf_dup = sdf.loc[sdf.duplicated(subset=["datetime_data"], keep=False)] # all lines
    sdf_dup = sdf.loc[
        sdf.duplicated(subset=["datetime_data"], keep="last")
    ]  # keep last dup
    print(f"There are {len(sdf_dup)} duplicates in this track")

    # remove all rows with duplicate times, prefer the one with best location accuracy
    sdf.drop_duplicates(
        subset=["datetime_data"], keep="last", inplace=True, ignore_index=True
    )

    # this should be true (check!)
    assert sdf[
        "datetime_data"
    ].is_monotonic_increasing, "Issue with timestamps, sort data!"

    return sdf


def track_speed(sdf):
    """
    Calculate speed, direction, distance and back azimuth between iceberg positions.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.  Note: clean and order it first!

    Returns
    -------
    sdf : Pandas dataframe
        Standardized track data - now with speed, etc.

    """
    # logger.info("Executing: calculate_velocity()")

    # Ensure rows are sorted by datetime.
    assert sdf[
        "datetime_data"
    ].is_monotonic_increasing, "Issue with timestamps, sort data!"

    # Initialize pyproj with appropriate ellipsoid
    geodesic = pyproj.Geod(ellps="WGS84")

    # Calculate forward azimuth and great circle distance between modelled coordinates
    sdf["direction"], back_azimuth, sdf["distance"] = geodesic.inv(
        sdf["longitude"].shift().tolist(),
        sdf["latitude"].shift().tolist(),
        sdf["longitude"].tolist(),
        sdf["latitude"].tolist(),
    )

    # Convert azimuth from (-180° to 180°) to (0° to 360°)
    sdf["direction"] = ((sdf["direction"] + 360) % 360).round(2)

    # Calculate time delta between rows (in seconds)
    time_delta = sdf["datetime_data"].diff().dt.total_seconds()

    # Calculate speed in m/s
    sdf["speed"] = sdf["distance"] / time_delta

    # Round columns
    # df = df.round({"distance": 1, "speed": 2, "direction": 2}) # Not working?
    sdf["distance"] = sdf["distance"].round(0)
    sdf["direction"] = sdf["direction"].round(0)
    sdf["speed"] = sdf["speed"].round(3)

    return sdf


def track_speed_limit(sdf, threshold=10):
    """
    Remove gross speeding violations from data.

    Note the intent here is to remove only the very worst rows from datasets.  It is
    a very crude way to cut down on clearly wrong position data.  Note high speeds are
    due to inaccurate positions, but also inprecise positions over short periods of time.
    It is important to be careful.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.
    threshold : float, optional
        A threshold, beyond which rows are removed (m/s). The default is 10.

    Returns
    -------
    sdf : Pandas dataframe
        Standardized track data - now limited.


    """
    # needs to be in a loop since if there is a fly-away point, you have going out and coming back
    before = len(sdf)
    while (sdf["speed"] > threshold).any():
        sdf.drop(sdf[sdf["speed"] > threshold].index[0], inplace=True)
        sdf = track_speed(sdf)
    print(f"Removed {before - len(sdf)} rows due to speed limit violations")

    sdf.reset_index(drop=True)

    return sdf


def track_trim(sdf, track_start=None, track_end=None):
    """
    Trim a track to a specified start and end time.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.
    track_start : datetime, optional
        track start time (earlier values will be removed). The default is None (no trim).
    track_end : datetime, optional
        track end time (later values will be removed). The default is None (no trim).

    Returns
    -------
    sdf : Pandas dataframe
        Standardized track data - now trimmed.

    """
    if track_start:
        sdf.drop(sdf[sdf["datetime_data"] < track_start].index, inplace=True)

    if track_end:
        sdf.drop(sdf[sdf["datetime_data"] > track_end].index, inplace=True)

    sdf.reset_index(inplace=True)

    return sdf


def track_output(sdf, types=["csv"], path_output=".", file_output=None):
    """
    Output the track to a file.

    Note the default is a csv (non-spatial) format.  Other options include track points
    (_pt) or track lines (_ln) in a kml or gpkg file [NB: let's move on from shapefiles!].
    See types option below.   Note that there is no fancy styling of the data.

    Parameters
    ----------
    sdf : Pandas dataframe
        Standardized track data.  Note: clean, order, 'speed' and trim it first!
    types : list of str, optional
        list of output types to generate ['csv', 'pt_kml', 'ln_kml', 'pt_gpkg','ln_gpkg']. The default is 'csv'.
    path_output : str, optional
        Path to put the output. The default is the current directory
    file_output : str, optional
        filename of output. The default is None, which will autogenerate on the Beacon ID
    Returns
    -------
    None.

    """
    # logger.info("Executing: track_output")

    if not file_output:
        file_output = sdf.beacon_id[0]

    # Convert to GeoPandas dataframe
    gdf_pt = gpd.GeoDataFrame(
        sdf, geometry=gpd.points_from_xy(sdf["longitude"], sdf["latitude"])
    )

    # Set CRS
    gdf_pt.crs = "EPSG:4326"

    gdf_ln = gdf_pt.groupby(["beacon_id"])["geometry"].apply(
        lambda x: LineString(x.tolist())
    )

    # Set CRS
    gdf_ln.crs = "EPSG:4326"

    # output part
    if "csv" in types:
        # Write CSV file without index column
        sdf.to_csv(f"{os.path.join(path_output, file_output)}.csv", index=False)

    if "pt_gpkg" in types:
        gdf_pt.to_file(
            f"{os.path.join(path_output, file_output)}_pt.gpkg", driver="GPKG"
        )

    if "ln_gpkg" in types:
        gdf_ln.to_file(
            f"{os.path.join(path_output, file_output)}_ln.gpkg", driver="GPKG"
        )

    if "pt_kml" in types:
        gdf_pt.to_file(f"{os.path.join(path_output, file_output)}_pt.kml", driver="KML")

    if "ln_kml" in types:
        gdf_ln.to_file(f"{os.path.join(path_output, file_output)}_ln.kml", driver="KML")

    return


def read_spec_file(spec_file):
    """
    Read spec file and create a dataframe.

    Parameters
    ----------
    spec_file : str
        Full path to the spec_file.

    Returns
    -------
    spec_df : pandas dataframe
        Specs for all beacon models.

    """
    spec_df = pd.read_csv(spec_file)

    return spec_df


def read_meta_file(meta_file):
    """
    Read metadata file and create a dataframe.

    Parameters
    ----------
    meta_file : str
        Full path to the beacon metadata file.

    Returns
    -------
    mdf : pandas dataframe
        Metadata for all tracks.

    """
    mdf = pd.read_csv(meta_file)

    return mdf


def track_logger(raw_file, path_output, level="DEBUG"):
    """
    Set up the logging.

    Parameters
    ----------
    raw_file : str
        path of the raw data file
    path_output : str
        path to the output file
    level : str, optional
        the logging level for the file. The default is "DEBUG".

    Returns
    -------
    track_log : logging.Logger
        An instance of the logger class

    """
    # create a name for the file
    loggerFileName = f"{Path(raw_file).stem}.log"

    # add full path here so it goes to the right place
    loggerFileName = os.path.join(path_output, loggerFileName)

    # assign the log level here, defaults to DEBUG, note there is no CRITICAL level
    match level.lower():
        case "debug":
            loglevel = logging.DEBUG
        case "info":
            loglevel = logging.INFO
        case "warning":
            loglevel = logging.WARNING
        case "error":
            loglevel = logging.ERROR
        case _:
            loglevel = logging.DEBUG

    # Create a logger instance here - it will be named after the module name (__main__?)
    track_log = logging.getLogger(__name__)
    track_log.setLevel(logging.DEBUG)  # sets the default level for this logger

    # Create handlers - these control output from the logger
    # stream handler - output to the console
    c_handler = logging.StreamHandler()
    # file handler - output to a file
    f_handler = logging.FileHandler(loggerFileName)

    # this sets the logging level for both handlers:
    c_handler.setLevel(logging.INFO)
    f_handler.setLevel(loglevel)

    # Create formatters and add them to handlers - this gives control over output
    c_format = logging.Formatter("%(message)s")
    f_format = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    c_handler.setFormatter(c_format)
    f_handler.setFormatter(f_format)

    # Add handlers to the logger
    track_log.addHandler(c_handler)
    track_log.addHandler(f_handler)

    # the following is a test:
    track_log.debug(
        "This is a debug test - ignore..."
    )  # should go to file but not to screen

    return track_log


def read_args():
    """
    Read arguments from command line, checks them, reads certain files.

    This function facilitates the command line operation of the workflow. Note that most
    of the arguments are not required, since they have defaults. There are some choices:
        The use can specify the standardization function (fn_std), beacon model, track
        start and end times, or leave them blank (which only works in some specific cases),
        or provide the path to the metadata file.  If the metdata file is present, the
        arguments mentioned above will be overwritten.

    Returns
    -------
    a list of the arguments.

    """
    prog_description = "Beacon track standardization and cleaning functions"
    parser = argparse.ArgumentParser(prog_description)

    # these are the required parameters - all others have default values
    parser.add_argument("raw_file", help="enter the raw data file")
    parser.add_argument(
        "output_path", help="enter the path to write the output file to"
    )
    # These keywords are not obligatory.
    parser.add_argument(
        "-f",
        "--fn_std",
        type=str,
        default="fn_standard",
        help="provide the name of the standardization function",
    )
    parser.add_argument(
        "-m",
        "--model",
        type=str,
        default="Default",
        help="the beacon model name (must be exact)",
    )
    parser.add_argument(
        "-sf",
        "--spec_file",
        type=str,
        default=None,
        help="the path/name of the specifications csv file",
    )
    parser.add_argument(
        "-s",
        "--track_start",
        type=str,
        default=None,
        help="the date-time of the track start in UTC (format: yyyy-mm-dd HH:MM:SS)",
    )
    parser.add_argument(
        "-e",
        "--track_end",
        type=str,
        default=None,
        help="the date-time of the track end in UTC (format: yyyy-mm-dd HH:MM:SS)",
    )
    parser.add_argument(
        "-mf",
        "--meta_file",
        type=str,
        default=None,
        help="the path/name of the metadata csv file. Note that the script will seek \
            fn_std, model, track_start, track_end in the meta_file",
    )
    parser.add_argument(
        "-of",
        "--output_file",
        type=str,
        default=None,
        help="the name of the standardized and trimmed track file",
    )
    parser.add_argument(
        "-ot",
        "--output_type",
        type=str,
        default="csv",
        nargs="+",
        choices={"csv", "pt_kml", "ln_kml", "pt_gpkg", "ln_gpkg"},
        help="list the output types to produce:  ; defaults to producing csv only",
    )

    args = parser.parse_args()

    # all the arguments here:
    raw_file = args.raw_file
    output_path = args.output_path
    fn_std = args.fn_std
    model = args.model
    spec_file = args.spec_file
    track_start = args.track_start
    track_end = args.track_end
    meta_file = args.meta_file
    output_file = args.output_file
    output_types = args.output_type

    # some attempt at error trapping early on....
    assert os.path.isfile(
        raw_file
    ), f"Raw data file: {raw_file} was not found. Please check and run again"
    assert os.path.isdir(
        output_path
    ), f"Output path: {output_path} was not found. Please check and run again"
    if spec_file:
        assert os.path.isfile(
            spec_file
        ), f"Spec file: {spec_file} was not found. Please check and run again"
    if meta_file:
        assert os.path.isfile(
            meta_file
        ), f"Meta file: {meta_file} was not found. Please check and run again"

    if os.path.basename(raw_file) == output_file:
        if Path(raw_file).parent == output_path:
            print(
                "The output file you specified will overwrite the raw data file, please fix and re-run"
            )
            sys.exit(1)

    # if there is a meta_file, then open it and replace the following parameters
    if meta_file:
        mdf = read_meta_file(meta_file)
        fn_std = mdf.standardization_fn.loc[mdf.beacon_id == Path(raw_file).stem].iloc[
            0
        ]
        model = mdf.beacon_model.loc[mdf.beacon_id == Path(raw_file).stem].iloc[0]
        if "track_start" in mdf:
            track_start = pd.to_datetime(
                mdf.track_start.loc[mdf.beacon_id == Path(raw_file).stem], utc=True
            ).iloc[0]
        if "track_end" in mdf:
            track_end = pd.to_datetime(
                mdf.track_end.loc[mdf.beacon_id == Path(raw_file).stem], utc=True
            ).iloc[0]

    # check track start and end
    if track_start:
        try:
            track_start = pd.to_datetime(track_start, utc=True)
        except:
            print("Unrecognized track start format")
            sys.exit(1)

    if track_end:
        try:
            track_end = pd.to_datetime(track_end, utc=True)
        except:
            print("Unrecognized track end format")
            sys.exit(1)

    # read in the spec file
    if spec_file:
        specs_df = read_spec_file(spec_file)
    else:
        specs_df = None

    return [
        raw_file,
        output_path,
        fn_std,
        model,
        specs_df,
        track_start,
        track_end,
        output_file,
        output_types,
    ]


def track_process(
    raw_file,
    output_path,
    fn_std,
    model,
    specs_df,
    track_start,
    track_end,
    output_file,
    output_types,
):
    """
    Process a raw track: standardize, clean, trim and output.

    Parameters
    ----------
    raw_file : TYPE
        DESCRIPTION.
    output_path : TYPE
        DESCRIPTION.
    fn_std : TYPE
        DESCRIPTION.
    model : TYPE
        DESCRIPTION.
    specs_df : TYPE
        DESCRIPTION.
    track_start : TYPE
        DESCRIPTION.
    track_end : TYPE
        DESCRIPTION.
    output_file : TYPE
        DESCRIPTION.
    output_types : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """
    log = logging.getLogger("log")
    log.info(f"Reading and standardizing raw data file: {raw_file}")
    track = track_read(raw_file, fn_std)
    if type(specs_df) == pd.core.frame.DataFrame:  # no point cleaning without the specs
        log.info("Cleaning the track")
        track = track_clean(track, model, specs_df)
    log.info("Ordering the track")
    track = track_order(track)
    log.info("Speeding the track")
    track = track_speed(track)
    log.info("Removing speed violations")
    track = track_speed_limit(track)
    log.info(
        f"Trimming the track at start: {track_start.isoformat() if track_start else 'NA'} \
            and end: {track_end.isoformat() if track_end else 'NA'}"
    )
    track = track_trim(track, track_start=track_start, track_end=track_end)
    log.info(
        f"Outputting the track as {os.path.basename(raw_file) if output_file else output_file}\
            to {output_path} as {' '.join(output_types)}"
    )
    track_output(track, output_types, path_output=output_path, file_output=output_file)
    log.info("Completed track... ")
    print(
        f"Beacon {Path(raw_file).stem} from: {track.datetime_data.iloc[0].isoformat()} \
          to: {track.datetime_data.iloc[-1].isoformat()}"
    )


def main():
    """
    Main function.

    """
    import pdb

    pdb.set_trace()
    # read in arguments from the command line.
    (
        raw_file,
        output_path,
        fn_std,
        model,
        specs_df,
        track_start,
        track_end,
        output_file,
        output_types,
    ) = read_args()

    log = track_logger(raw_file, output_path, level="INFO")

    log.info(f":...{Path(raw_file).stem}....\n")

    track_process(
        raw_file,
        output_path,
        fn_std,
        model,
        specs_df,
        track_start,
        track_end,
        output_file,
        output_types,
    )


if __name__ == "__main__":
    main()


# # Problem IDs:
# 2009_300034012571050
# 2010_300034012592660

# # Generate database
# create_database(path_input)
