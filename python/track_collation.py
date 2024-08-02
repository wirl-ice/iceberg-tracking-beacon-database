#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
track_collation.py

Batch process many tracks and/or collate many tracks into a larger database 


Created on Sun Jul 28 04:33:24 2024

@author: dmueller
"""

import os
import track_processing
from pathlib import Path

meta_file = (
    "/home/dmueller/Desktop/cis_iceberg_beacon_database_0.3/database/ITBDMetadata.csv"
)
spec_file = (
    "/home/dmueller/Desktop/cis_iceberg_beacon_database_0.3/database/BeaconSpecs.csv"
)
scandir = "/home/dmueller/Desktop/cis_iceberg_beacon_database_0.3/raw_data/"
outdir = "/home/dmueller/Desktop/cis_iceberg_beacon_database_0.3/test/"

# note this will remove all files and folders from ... press y to continue


# go down the tree..
# at each level, create the same folder structure as we have in scandir
# at each level, get basename of the files and compare with meta data file beacon_id
# if match, then run the proceess.
# put the output in the copied folder structure

mdf = track_processing.read_meta_file(meta_file)
specs_df = track_processing.read_spec_file(spec_file)
# split the filenames on "_" and if there are 2 parts, then

scandir = os.path.abspath(scandir)
outdir = os.path.abspath(outdir)
prefix = len(scandir) + len(os.path.sep)

if not os.path.isdir(outdir):
    os.makedirs(outdir)

for root, dirs, files in os.walk(scandir, topdown=True):
    for d in dirs:
        if not os.path.isdir(os.path.join(outdir, root[prefix:], d)):
            os.mkdir(os.path.join(outdir, root[prefix:], d))
    for f in files:
        if Path(f).stem in mdf.beacon_id.values:
            print(f"\n\n......{f}.......")
            track_processing.track_process(
                os.path.join(root, f),
                os.path.join(outdir, root[prefix:]),
                mdf.standardization_fn.loc[mdf.beacon_id == Path(f).stem].iloc[0],
                mdf.beacon_model.loc[mdf.beacon_id == Path(f).stem].iloc[0],
                specs_df,
                None,
                None,
                None,
                "csv",
            )
