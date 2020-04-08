import csv
import os
import glob
import shutil

# Import CSV files from folder
#my_path = os.getcwd()
my_path = '/Users/adam/Desktop/cis_iceberg_beacon_database/data' # Change accordingly
files = sorted(glob.glob(my_path + '/**/standardized_data/*.csv', recursive=True)) # Requires Python 3

# Concatenate CSV files
with open('output.csv', 'w') as outfile:
    for i, file in enumerate(files):
        with open(file, 'r') as infile:
            if i != 0:
                infile.readline()  # Throw away header on all but first file
            # Block copy rest of file from input to output without parsing
            shutil.copyfileobj(infile, outfile)
            print(file + " has been imported.")