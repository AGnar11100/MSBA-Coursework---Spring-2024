import os
from pyspark import SparkContext

# Initialize SparkContext
sc = SparkContext(appName="NCDCReducer")

# Read the output from the mapper
year_temperature = sc.textFile("output/year_temperature/*")

# Reduce by key to aggregate temperature data for each year
filtered_temperature = year_temperature.map(lambda line: line.split()) \
                                         .map(lambda x: (int(x[0].strip(" ),(")), int(x[1].strip(" ),(")))) \
                                         .filter(lambda x: x[0] != 0 and x[1] != 0) \
                                         .filter(lambda x: x[1] < 135 and x[1] > -62)

# Write aggregated temperature data to a text file
aggregated_temperature = filtered_temperature.map(lambda x: "{},{}".format(x[0], x[1])) \
                      .repartition(1).saveAsTextFile("output/aggregated_temperature/")



# Stop SparkContext
sc.stop()
