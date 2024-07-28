#!/usr/bin/env python

import sys
import gzip

# Import PySpark modules
from pyspark import SparkContext

# Initialize SparkContext
sc = SparkContext(appName="NCDCMapper")

# Function to parse NCDC record and extract year and temperature
def parse_ncdc_record(record):
    MISSING = 9999
    line = record.strip()
    year = line[15:19]
    airTemperature = 0
    if len(line) >= 93:
        if line[87] == '+':
            airTemperature = int(line[88:92])
        else:
            airTemperature = int(line[87:92])
    quality = ""
    if len(line) >= 93:
        quality = line[92:93]
    if airTemperature != MISSING and quality in ['0', '1', '4', '5', '9']:
        return int(year), airTemperature
    else:
	return 0, 0  



# Read all .gz files in the CourseProjectDataset directory
file_contents = sc.wholeTextFiles("CourseProjectData/*.gz")

# Map each file content to (year, temperature) tuples
year_temperature = file_contents.flatMap(lambda file_content: map(parse_ncdc_record, file_content[1].split('\n')))

# Save the result as text file
year_temperature.saveAsTextFile("output/year_temperature")

# Stop SparkContext
sc.stop()
