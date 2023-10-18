#
# Python script to recursively rename folders
#
# Author: Adam Garbo
#
# Date: April 7, 2020
#

import os

path = "/Volumes/data/iceberg_tracking_beacon_database"


for path, dirs, files in os.walk(
    "/Users/adam/Desktop/iceberg_tracking_beacon_database/data"
):
    for dir in dirs:
        if dir == "Documentation":
            newDirN = "documentation"
        elif dir == "Photos":
            newDirN = "photos"
        elif dir == "RawData":
            newDirN = "raw_data"
        elif dir == "RawData":
            newDirN = "raw_data"
        elif dir == "Rawdata":
            newDirN = "raw_data"
        elif dir == "StandardizedData":
            newDirN = "standardized_data"
        elif dir == "Standardizeddata":
            newDirN = "standardized_data"
        elif dir == "Deployment":
            newDirN = "deployed_file"
        elif dir == "OriginalFile":
            newDirN = "original_file"
        elif dir == "Metadata":
            newDirN = "metadata"
        else:
            continue

        newDir = os.path.join(path, newDirN)
        oldDir = os.path.join(path, dir)
        os.rename(oldDir, newDir)

# Script to recursively rename a single folder
import os

path = "/Users/adam/Desktop/iceberg_tracking_beacon_database/data"

for path, dirs, files in os.walk(path):
    for dir in dirs:
        if dir == "deployed_file":
            newDirN = "deployment_file"
        else:
            continue

        newDir = os.path.join(path, newDirN)
        oldDir = os.path.join(path, dir)
        os.rename(oldDir, newDir)


# Use the following code to search for files
folders = []

for root, dirs, folder in os.walk(path):
    for d in dirs:
        if d.find("standardized_data/*") >= 0:
            folders.append(os.path.join(root, d))

for f in folders:
    print(f)


# Use the following code only to view all the folders
folders = []

for root, dirs, folder in os.walk(path):
    for d in dirs:
        folders.append(os.path.join(root, d))

for f in folders:
    print(f)
