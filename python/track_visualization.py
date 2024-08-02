#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
track_visualization.py

Functions to produce visualizations of beacon data that
has been ingested, cleaned, and standardized. 

Visualizations include: 
    map - a map of the iceberg track
    time - variables over time - temperature, displacement and velocity
    dist - statistical distributions - polar plot of direction, histogram of speed
    temp - temperature changes (used for checking if beacon is still on target)

Author: Adam Garbo, December 2022
Modified: Derek Mueller, July 2024
"""
import os
import sys
import logging
import argparse
from pathlib import Path
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.dates as mdates
import cartopy.crs as ccrs
import cartopy.feature as cfeature


def track_map(std_file, path_output=".", dpi=300):
    """
    Create a map of the iceberg track.

    Map is saved as a png, the star indicates the start

    Parameters
    ----------
    std_file : str
        Standardized beacon track file.
    path_output : str, optional
        Path to save output. The default is ".".
    dpi : int, optional
        Resolution of the graph in dots per inch. The default is 300.

    Returns
    -------
    None.

    """
    # Load standardized CSV file
    df = pd.read_csv(std_file, index_col=False)

    # Convert datetime
    df["datetime_data"] = pd.to_datetime(df["datetime_data"])

    # Add Natural Earth coastline
    coast = cfeature.NaturalEarthFeature(
        "physical", "land", "10m", edgecolor="black", facecolor="lightgray", lw=0.5
    )

    # Set map centres
    x = df["longitude"].median()
    y = df["latitude"].median()

    # Plot latitude and longitude
    fig = plt.figure(figsize=(10, 10), constrained_layout=True)
    ax = plt.axes(projection=ccrs.Orthographic(x, y))
    ax.add_feature(coast)
    ax.set_adjustable("datalim")
    gl = ax.gridlines(
        crs=ccrs.PlateCarree(),
        draw_labels=True,
        color="black",
        alpha=0.25,
        linestyle="dotted",
        x_inline=False,
        y_inline=False,
    )
    gl.rotate_labels = False
    gl.top_labels = False
    gl.right_labels = False
    gl.xpadding = 5

    sns.scatterplot(
        x="longitude",
        y="latitude",
        data=df,
        linewidth=1,
        edgecolor="black",
        legend=False,
        transform=ccrs.PlateCarree(),
    )

    # plot the very start of the track
    plt.plot(
        df.longitude.iloc[0], df.latitude.iloc[0], marker="*", ms=20, mfc="r", mec="k"
    )

    # Calculate variables for annotations
    beacon_id = df["beacon_id"][0]
    start_date = df["datetime_data"].iloc[0]
    end_date = df["datetime_data"].iloc[-1]
    duration = df["datetime_data"].iloc[-1] - df["datetime_data"].iloc[0]
    duration = round(duration.days + duration.seconds / (24 * 60 * 60), 2)
    distance = round(df["distance"].sum() / 1000, 2)
    observations = len(df.index)

    fig.suptitle(
        f"{beacon_id} map", fontweight="bold", fontsize=18
    )  # note use y= to adjust

    ax.text(
        0,
        -0.09,
        f"Start (*): {start_date} UTC, End: {end_date} UTC\nDuration: {duration:.2f} days, Distance: {distance:,.2f} km, Observations: {observations:,}",
        transform=ax.transAxes,
        color="black",
        fontsize=12,
        fontweight="regular",
    )

    # plt.show()

    plt.savefig(
        os.path.join(
            path_output, f"{os.path.splitext(os.path.basename(std_file))[0]}_map.png"
        ),
        dpi=dpi,
        transparent=False,
        bbox_inches="tight",
    )
    plt.close()


def track_temp(std_file, path_output=".", dpi=300):
    """
    Create a graph of the beacon temperature.

    There are 3 panels:
        Temperature (of the air, surface or internal, depending on what is available)
        7 day rolling mean of temperature
        7 day rolling standard deviation of temperature

    This is useful for finding the start and end of the track:
        When beacons are deployed they may still be acclimatizing to ambient conditions
        When beacons fall off their target into the water, the standard deviation should
            decrease.  Temperature should approach water temperature (typically decreasing)

    Parameters
    ----------
    std_file : str
        Standardized beacon track file.
    path_output : str, optional
        Path to save output. The default is ".".
    dpi : int, optional
        Resolution of the graph in dots per inch. The default is 300.

    Returns
    -------
    None.

    """
    # Load standardized CSV file
    df = pd.read_csv(std_file, index_col=False)

    # Convert datetime
    df["datetime_data"] = pd.to_datetime(df["datetime_data"])

    # get the temperature
    df["temperature"] = df["temperature_air"]
    if df["temperature_air"].isnull().all():
        df["temperature"] = df["temperature_surface"]
        if df["temperature_surface"].isnull().all():
            df["temperature"] = df["temperature_internal"]
            if df["temperature_internal"].isnull().all():
                df["temperature"] = 0

    df["Tmean"] = df.rolling(
        window="7d", on="datetime_data", center=True
    ).temperature.mean()
    df["Tstd"] = df.rolling(
        window="7d", on="datetime_data", center=True
    ).temperature.std()

    # plotting  - t temp, m mean, s std
    fig, (t, m, s) = plt.subplots(
        3, 1, figsize=(10, 8), sharex=True, constrained_layout=True
    )

    # Temperature plot
    t.grid(ls="dotted")
    sns.lineplot(
        ax=t, x="datetime_data", y="temperature", data=df, errorbar=None, color="b"
    )
    t.set(xlabel=None, ylabel="Temperature (°C)")

    # Rolling temperature mean plot
    m.grid(ls="dotted")
    sns.lineplot(ax=m, x="datetime_data", y="Tmean", data=df, errorbar=None, color="r")
    m.set(xlabel=None, ylabel="Temp. rolling mean (°C)")

    # Rolling temperature std plot
    s.grid(ls="dotted")
    sns.lineplot(ax=s, x="datetime_data", y="Tstd", data=df, errorbar=None, color="k")
    s.set(xlabel=None, ylabel="Temp. rolling std (°C)")
    plt.xticks(rotation=45, horizontalalignment="center")

    # Calculate variables for annotations
    beacon_id = df["beacon_id"][0]
    start_date = df["datetime_data"].iloc[0]
    end_date = df["datetime_data"].iloc[-1]
    duration = df["datetime_data"].iloc[-1] - df["datetime_data"].iloc[0]
    duration = round(duration.days + duration.seconds / (24 * 60 * 60), 2)
    distance = round(df["distance"].sum() / 1000, 2)
    observations = len(df.index)

    fig.suptitle(f"{beacon_id} temperature plot", fontweight="bold", fontsize=18)

    s.text(
        0,
        -0.61,
        f"Start: {start_date} UTC, End: {end_date} UTC\nDuration: {duration:.2f} days, Distance: {distance:,.2f} km, Observations: {observations:,}",
        transform=s.transAxes,
        color="black",
        fontsize=12,
        fontweight="regular",
    )

    fig.align_ylabels()
    # plt.show()

    plt.savefig(
        os.path.join(
            path_output, f"{os.path.splitext(os.path.basename(std_file))[0]}_temp.png"
        ),
        dpi=dpi,
        transparent=False,
        bbox_inches="tight",
    )
    plt.close()


def track_dist(std_file, path_output=".", dpi=300):
    """
    Create a graph of the track's statistical distributions.

    Parameters
    ----------
    std_file : str
        Standardized beacon track file.
    path_output : str, optional
        Path to save output. The default is ".".
    dpi : int, optional
        Resolution of the graph in dots per inch. The default is 300.

    Returns
    -------
    None.

    """
    # logger.info("Executing: visualize_data()")

    # Load standardized CSV file
    df = pd.read_csv(std_file, index_col=False)

    # Convert datetime
    df["datetime_data"] = pd.to_datetime(df["datetime_data"])

    fig = plt.figure(figsize=(10, 5))  # , constrained_layout=True)
    h = plt.subplot(121)
    p = plt.subplot(122, projection="polar")

    # %% Histogram of the speed
    # clean data -- this should not be needed, so commented out.
    # unreasonably high >2.8 m/s Garbo, > 8 m/s Dalton  Dalton fastest was 2.3 m/s
    # df.replace([np.inf, -np.inf], np.nan, inplace=True)
    # df.loc[df["speed"] > 5] = np.nan  # there is no way an iceberg moves faster than 5 m/s

    # Create histogram
    # here assuming that there will not be any speeds greater than 2.5 m/s (i.e., more constrained)
    # by setting the same lower and upper limit, the histograms are comparable
    xlowerlim = 0
    xupperlim = 2.5
    nbins = int(10 * (xupperlim - xlowerlim) / 0.5)  # this sets the number of bins
    # Here bins start with 0 to 0.05 m/s. That represents a displacement of 180 m so there is
    # likely true motion within that bin.
    # Dalton examined a stationary beacon and found mean + 1 SD =
    # 0.04 m/s.

    # calcuate bins and values
    values, bins = np.histogram(
        df.speed[~np.isnan(df.speed)], range=(xlowerlim, xupperlim), bins=nbins
    )
    # convert to fraction of all obs
    frac = values / len(df.speed[~np.isnan(df.speed)])
    # cumfrac = np.cumsum(frac)

    # here we set the y axis (left) limits - they represent fraction of obs within each bin
    ylowerlim = 0
    yupperlim = 1

    h.set_xlabel("Speed (m/s)")
    h.set_ylabel("Fraction of observations", color="red")
    h.hist(bins[:-1], bins, weights=frac, color="red")
    h.tick_params(axis="y", labelcolor="red")
    h.set_ylim([ylowerlim, yupperlim * 1.01])
    h.set_xlim([xlowerlim, xupperlim])

    # plot cummulative hist on right y axis
    h2 = h.twinx()

    h2.set_ylabel(
        "Cumulative fraction of observations", color="blue", rotation=270, labelpad=15
    )
    h2.hist(
        bins[:-1], bins, weights=frac, color="blue", histtype="step", cumulative=True
    )
    h2.tick_params(axis="y", labelcolor="blue")
    h2.set_ylim([ylowerlim, yupperlim * 1.01])

    # %%  Polar plot of speed and direction
    p.plot(
        df.direction,
        df.speed,
        marker="o",
        markerfacecolor="b",
        markeredgecolor="k",
        linestyle="None",
    )
    p.grid(True)
    p.set_theta_direction(-1)  # turn clockwise
    p.set_theta_zero_location("N")  # north up
    p.set_rlabel_position(200.5)  # Move radial labels away from plotted line
    plt.text(0.05, -0.05, "Speed (m/s)", transform=p.transAxes)

    # Calculate variables for annotations
    beacon_id = df["beacon_id"][0]
    start_date = df["datetime_data"].iloc[0]
    end_date = df["datetime_data"].iloc[-1]
    duration = df["datetime_data"].iloc[-1] - df["datetime_data"].iloc[0]
    duration = round(duration.days + duration.seconds / (24 * 60 * 60), 2)
    distance = round(df["distance"].sum() / 1000, 2)
    observations = len(df.index)

    fig.suptitle(f"{beacon_id} distribution plot", fontweight="bold", fontsize=18)

    h.text(
        0.0,
        -0.275,
        f"Start: {start_date} UTC, End: {end_date} UTC\nDuration: {duration:.2f} days, Distance: {distance:,.2f} km, Observations: {observations:,}",
        transform=h.transAxes,
        color="black",
        fontsize=12,
        fontweight="regular",
    )

    plt.subplots_adjust(wspace=0.3)
    # plt.show()

    plt.savefig(
        os.path.join(
            path_output, f"{os.path.splitext(os.path.basename(std_file))[0]}_dist.png"
        ),
        dpi=dpi,
        transparent=False,
        bbox_inches="tight",
    )

    plt.close()


def track_time(std_file, path_output=".", dpi=300):
    """
    Create a graph of variables with respect to time.

    There are 3 panels:
        Temperature (of the air, surface or internal, depending on what is available)
        Distance (scatter plot)
        Speed and direction (quiver)

    TODO: Subsample to a daily timestep?

    Parameters
    ----------
    std_file : str
        Standardized beacon track file.
    path_output : str, optional
        Path to save output. The default is ".".
    dpi : int, optional
        Resolution of the graph in dots per inch. The default is 300.

    Returns
    -------
    None.

    """
    # Load standardized CSV file
    df = pd.read_csv(std_file, index_col=False)

    # Convert datetime
    df["datetime_data"] = pd.to_datetime(df["datetime_data"])

    # get the temperature
    df["temperature"] = df["temperature_air"]
    if df["temperature_air"].isnull().all():
        df["temperature"] = df["temperature_surface"]
        if df["temperature_surface"].isnull().all():
            df["temperature"] = df["temperature_internal"]
            if df["temperature_internal"].isnull().all():
                df["temperature"] = 0

    # plotting
    fig, (t, d, q) = plt.subplots(
        3, 1, figsize=(10, 8), sharex=True, constrained_layout=True
    )

    # Temperature plot
    t.grid(ls="dotted")
    sns.lineplot(
        ax=t, x="datetime_data", y="temperature", data=df, errorbar=None, color="b"
    )
    t.set(xlabel=None, ylabel="Temperature (°C)")

    # Distance plot
    sns.lineplot(
        ax=d, x="datetime_data", y="distance", data=df, errorbar=None, color="r"
    )
    d.set(xlabel=None, ylabel="Displacement (m)")

    # quiver plot
    u = df.speed * np.sin(np.radians(df.direction))
    v = df.speed * np.cos(np.radians(df.direction))

    q.quiver(df["datetime_data"], 0, u, v)
    q.set(xlabel=None, ylabel="Velocity (m/s)")
    fig.align_ylabels()
    plt.xticks(rotation=45, horizontalalignment="center")

    # Calculate variables for annotations
    beacon_id = df["beacon_id"][0]
    start_date = df["datetime_data"].iloc[0]
    end_date = df["datetime_data"].iloc[-1]
    duration = df["datetime_data"].iloc[-1] - df["datetime_data"].iloc[0]
    duration = round(duration.days + duration.seconds / (24 * 60 * 60), 2)
    distance = round(df["distance"].sum() / 1000, 2)
    observations = len(df.index)

    fig.suptitle(f"{beacon_id} time plot", fontweight="bold", fontsize=18)

    q.text(
        0,
        -0.61,
        f"Start: {start_date} UTC, End: {end_date} UTC\nDuration: {duration:.2f} days, Distance: {distance:,.2f} km, Observations: {observations:,}",
        transform=q.transAxes,
        color="black",
        fontsize=12,
        fontweight="regular",
    )

    # plt.show()

    plt.savefig(
        os.path.join(
            path_output, f"{os.path.splitext(os.path.basename(std_file))[0]}_time.png"
        ),
        dpi=dpi,
        transparent=False,
        bbox_inches="tight",
    )
    plt.close()


def main():
    """Work from command line."""
    # get parameters from command line:
    parser = argparse.ArgumentParser(description="Beacon track visualization functions")
    parser.add_argument("std_file", help="enter full path to the standard data file")
    parser.add_argument(
        "-p",
        "--path_output",
        help="enter path to store output png files ; \
                        defaults to the current directory",
    )
    parser.add_argument(
        "-g",
        "--graphs",
        nargs="+",
        choices={"map", "time", "temp", "dist"},
        help="list the graphs to produce: map time temp dist ; defaults to producing ALL graphs",
    )
    args = parser.parse_args()

    std_file = args.std_file
    path_output = args.path_output
    graphs = args.graphs

    # validate the parameters

    if not os.path.isfile(std_file):
        print(f"{std_file} not found, exiting...")
        sys.exit(1)

    if path_output:
        if not os.path.isdir(path_output):
            print(f"{path_output} not found, exiting... ")
            sys.exit(1)
    else:
        print(
            f"Output directory not requested, defaulting to current directory: {os.getcwd()}..."
        )
        path_output = os.getcwd()

    if graphs:
        if "map" in graphs:
            track_map(std_file, path_output=path_output)
        if "dist" in graphs:
            track_dist(std_file, path_output=path_output)
        if "temp" in graphs:
            track_temp(std_file, path_output=path_output)
        if "time" in graphs:
            track_time(std_file, path_output=path_output)

    else:  # if no graphs are requested, then produce them all
        track_map(std_file, path_output=path_output)
        track_temp(std_file, path_output=path_output)
        track_dist(std_file, path_output=path_output)
        track_time(std_file, path_output=path_output)


if __name__ == "__main__":
    main()

# std_file = "/home/dmueller/Desktop/cis_iceberg_beacon_database_0.3/standardized_data/2010/300034012592660/2010_300034012592660.csv"

# output = "/home/dmueller/Desktop"
# path_output = output

# # Get standardized data output path
# path_output = Path(file).resolve().parents[0]

# # Get unique beacon ID
# filename = Path(file).stem


# # -----------------------------------------------------------------------------
# # Configure logging
# # -----------------------------------------------------------------------------

# logger = logging.getLogger(__name__)
# logger.setLevel(logging.INFO)
# formatter = logging.Formatter(
#     "%(asctime)s:%(msecs)d %(name)s %(levelname)s %(message)s", "%Y-%m-%d %H:%M:%S"
# )
