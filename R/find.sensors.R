##--------------------------------Find-sensors--------------------------------##
## This script can be used to remove the already recovered sensors from the list
## of all sensors that have been located in the field. This script will create a
## *.KML file (saved in the working directory), which can be opened with Google.
## This *.KML file can be converted to a *.GPX file (default format in most GPS)
## in order to be able to find the remaining sensors in them field with the GPS.
##---------------------------------v.01-09-14---------------------------------##

# Load libraries & set the workspace

setwd ("~/thesis/data/GIS/")               # folder where the csv is located   
registry <- "iButtonIDs.csv"               # name of the file with locations
library(rgdal)
library(sp)

# Read the file of all the sensors that have been placed

loc <- read.csv(registry) 

# Enter the numer of the sensors that have been found (see 'loc'- file).
# Enter the 6-digit numbers within quotation marks, seperated by comma.

recovered.sensors  <-  c("33760F", "337614")

# Remove the recovered sensors from the list of all the placed sensors.

loc <- loc[ ! loc$Number %in% nm, ]
l <- loc

# Create spatial objects from the sensors (*.KML and *.GPX files)

coordinates(l) = ~X+Y
sensors <- SpatialPointsDataFrame(l, loc)
proj4string(sensors) = "+proj=longlat +datum=WGS84"
writeOGR(sensors, dsn="sensores_left.kml", layer= "sensors", driver="KML")
# Go to http://kml2gpx.com/ to convert the KML file to a GPX object.

