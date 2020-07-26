import csv
import os
import glob
import shutil
import time

datetime = time.strftime("%Y%m%d")
filename = 'database_' + datetime + '.csv'

# Import CSV files from folder
#my_path = os.getcwd()
input_path = '/Users/adam/Desktop/cis_iceberg_beacon_database/data' # Change accordingly
output_path = '/Users/adam/Desktop/cis_iceberg_beacon_database/output_data/csv/' + filename # Change accordingly
files = sorted(glob.glob(input_path + '/**/standardized_data/*.csv', recursive=True)) # Requires Python 3


# Concatenate CSV files
with open(output_path, 'w') as outfile:
    for i, file in enumerate(files):
        with open(file, 'r') as infile:
            if i != 0:
                infile.readline()  # Throw away header on all but first file
            # Block copy rest of file from input to output without parsing
            shutil.copyfileobj(infile, outfile)
            print(file + " has been imported.")