# Python

`beacon_processing`
* Main Python script that contains functions to process raw beacon trajectory data
  * Recursively searches through all database folders to identify files to process
  * Selects appropriate conversion function
  * Standardizes data columns
  * Cleans data according to minimum/maximum values
  * Calculates velocity
  * Creates output files

`processing_functions.py`
* Contains Python functions to convert specific beacon types

'rename_folders.py'
* Can be used to recursively search through the entire database and rename folders as desired

