#------------------------------------------------------------------------------
# Title: Metadata generation
#
# Created by: 
#
# Modified by: 
#   Adam Garbo, June 18, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Script to import metadata stored in csv file (one row per unique beacon ID) 
#   and output individual metadata files for each beacon
#   
# Required file(s): 
#
# Notes: 
#   
#------------------------------------------------------------------------------

# Libraries
library(jsonlite)
library(XML)
source("write_json.R")

# Set working directory
dir <- setwd("~/Desktop/CIS/BeaconData/Icebergs_IceIslands_BeaconDatabase/collection_metadata")

# Import master csv file, ensure blank cells with no data are assigned NA
df <- read.csv('BeaconMetadata_standardizedheadings_March26.csv', na.strings="")

# Make a 'name' column that is beaconid_year so that names of metadata files will be consistent with data names
name <- paste(df$BeaconID, df$YearDeployed, sep="_")
df <-cbind(df, name)

# Write out each row as a separate text file and assign beacon name:
for(i in 1:nrow(df)) {
  write.table(df[i,], file=paste(df$name[i], "_metadata.txt", sep=""), row.names=FALSE, sep="\t")
}

text<- read.table("1104_2002_metadata.txt", header = TRUE)

# This works to write json files - just need to figure out how to loop it to write one 
# per csv line (or one per text file I already made. Also how to name properly)
write_json(text, "~/Desktop/CIS/BeaconData/Icebergs_IceIslands_BeaconDatabase/collection_metadata/test.json", pretty=TRUE)

