#
# Python script to recursively rename folders
#
# Author: Adam Garbo
#
# Date: April 7, 2020
#

import os

path = '/Users/adam/Desktop/test'

for path, dirs, files in os.walk(path):
    for dir in dirs:
        if dir == "Documentation":
            newDirN = "documentation"
        else:
            continue
        
        newDir = os.path.join(path,newDirN)
        oldDir = os.path.join(path,dir)
        os.rename(oldDir, newDir)