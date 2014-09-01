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

loc <- loc[ ! loc$Number %in% c(recovered.sensors), ]
l <- loc

# Create spatial objects from the sensors (*.KML and *.GPX files)

coordinates(l) = ~X+Y
sensors <- SpatialPointsDataFrame(l, loc)
proj4string(sensors) = "+proj=longlat +datum=WGS84"
writeOGR(sensors, dsn="sensors_left.kml", layer= "sensors", driver="KML")
# Go to http://kml2gpx.com/ to convert the KML file to a GPX object.

