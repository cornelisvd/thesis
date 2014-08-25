# After downloading the temperature and humidity logs (*.csv) in two seperate folders, data can be loaded.
# Two logs will be created, which will include a unique ID that has to be added in a seperate *.csv file.
# This file will have to include the ID and GPS coordinates of the iButtons in the right order for merging.
# A KML will be linked to the spatial dataframe for further spatial analysis of the spatial points dataframe.

library(sp)             # Used to create the coordinates for the sppatial data frames.
library(rgdal)          # Used for the projection and writing of the KML % SHP files.
library(rgeos)          # Used for the calculation of the area size (gArea and gDistance).
library(raster)         # Used for the retrieving of additional geographic data (coasts).
library(OpenStreetMap)  # Used to draw the study area and sampling points on certain map type. 
library(fields)         # Used for the interpolation (kriging) of a number of variables.

# Change the working directory below to the location where the 'ibuttons' folder is located on your drive.

setwd("~/ibuttons/")

# The observations of the ibuttons will be loaded en linked to the unique ID of the ibuttons in the *csv.

humdobs  <- list.files("humidity")
        humdlog  <- do.call("cbind", lapply(humdobs, FUN=function(files){subset(read.csv(paste(
                "humidity/", files, sep = ""), skip = 19, header = TRUE, nrows = 144), select = "Value")}))

tempobs  <- list.files("temperature")
        templog  <- do.call("cbind", lapply(tempobs, FUN=function(files){subset(read.csv(paste(
                "temperature/", files, sep = ""), skip = 19, header = TRUE, nrows = 144), select = "Value")}))

dateobs  <- subset(read.csv("humidity/1.csv", skip = 19), select = "Date.Time")
        datum <- strsplit(as.character(dateobs$Date.Time), " ")
                 datumdf <- data.frame(do.call(rbind, datum)) 
        hour <-  strsplit(as.character(datumdf$X2), ":")
                 hourdf <- data.frame(do.call(rbind, hour)) 

buttonID <- read.csv("ibuttons.csv")
        names(humdlog) <- buttonID$IBUTTON_ID
        names(templog) <- buttonID$IBUTTON_ID

rm(humdobs, tempobs, dateobs, datum, hour) 

# This block will create a table with hourly min, max, and mean temperature and humidity for an easy overview.
# Three graphs will be created, with min-max (1) temp and (2) humidity; and (3) mean temperature & humidity.

        meantemp  <- rowMeans(templog, na.rm = TRUE)
        mintemp   <- apply(templog, 1, min, na.rm = TRUE) 
        maxtemp   <- apply(templog, 1, max, na.rm = TRUE) 
        meanhum   <- rowMeans(humdlog, na.rm = TRUE)  
        minhum    <- apply(humdlog, 1, min, na.rm = TRUE) 
        maxhum    <- apply(humdlog, 1, max, na.rm = TRUE) 

                tempstat <- data.frame(datumdf, hourdf$X1, meantemp, mintemp, maxtemp)
                names(tempstat) <- c("Date", "Time", "Hour", "Mean", "Min", "Max")
                humdstat <- data.frame(datumdf, hourdf$X1, meanhum, minhum, maxhum) 
                names(humdstat) <- c("Date", "Time", "Hour", "Mean", "Min", "Max")

rm(maxhum, maxtemp, meanhum, meantemp, minhum, mintemp) 

        meantemp  <- aggregate(as.numeric(tempstat$Mean),  by = list(tempstat$Hour),  FUN = mean)
        mintemp   <- aggregate(as.numeric(tempstat$Min),   by = list(tempstat$Hour),  FUN = min)
        maxtemp   <- aggregate(as.numeric(tempstat$Max),   by = list(tempstat$Hour),  FUN = max) 
        meanhum   <- aggregate(as.numeric(humdstat$Mean),  by = list(humdstat$Hour),  FUN = mean)
        minhum    <- aggregate(as.numeric(humdstat$Min),   by = list(humdstat$Hour),  FUN = min)
        maxhum    <- aggregate(as.numeric(humdstat$Max),   by = list(humdstat$Hour),  FUN = max) 
        hournum   <- as.numeric(as.character(meantemp$Group.1))

climstat <- data.frame(hournum, meantemp$x, mintemp$x, maxtemp$x, meanhum$x, minhum$x, maxhum$x)
        colnames(climstat) <- c("Hour", "MeanT", "MinT", "MaxT", "MeanRH", "MinRH", "MaxRH")

rm(maxhum, maxtemp, meanhum, meantemp, minhum, mintemp, hournum) 

# Graph for min and max temperature at different times of day

plot(climstat$Hour, climstat$MaxT, type="l", col="red", main = "Diurnal temperature range (degrees Celcius)", 
        xlab = "Hour (local time)", ylab = "Temperature (C)", ylim=c(0,30)) 
                axis(1,at=c(1:24), labels=FALSE)
                lines(climstat$MinT, type="l", col="blue")
                legend(18, 29, c("Maximum", "Minimum"), lty=c(1,1), col=c("red","blue"))

# Graph for min and max humidity at different times of day

plot(climstat$Hour, climstat$MaxRH, type="p", col="red", main = "Diurnal humidity range (% RH)", 
         xlab = "Hour (local time)", ylab = "Relative humidity", ylim=c(0,100), pch=1)
                lines(climstat$MinRH, type="p", col="blue", pch=2)
                text(26, 15, "Minimum humidity", col = "blue",  srt = 270, xpd = TRUE)
                legend(19, 20, c("Maximum", "Minimum"), pch=c(1,2), col=c("red","blue"))

# Graph for min and max humidity at different times of day, this is a plot with to y-axis with different units.

par(mar=c(5, 4.5, 4, 5)) 
plot(climstat$Hour, climstat$MeanT, type="l", col="red", main = "Mean temperature (C) and humidity (% RH)", 
         xlab = " ", ylab = "Mean temperature", col.lab="red", ylim=c(0,30))
                par(new=TRUE)
barplot(climstat$MeanRH,xaxt="n",yaxt="n", ylab="", xlab="Hour (local time)", border="blue", col="NA", ylim=c(0,100))
                axis(4)
                text(33, 50, "Mean humidity", col = "blue",  srt = 270, xpd = TRUE)
                legend(22.5, 95, c("Temperature", "Humidity"), pch=c(15), col=c("red","blue"))

rm(meantemp, mintemp, maxtemp, hournum, meanhum, maxhum, minhum)

#This section will merge the *.csv with coordinates and the temperature/humidity data (on the iButton ID).

humdsw <- data.frame(t(humdlog)) 
        names(humdsw) <- paste("t", hourdf$X1, hourdf$X2, sep = "")
                humdsp <- merge(buttonID[1:3], humdsw, by.x = "IBUTTON_ID", by.y = "row.names")
tempsw <- data.frame(t(templog))
        names(tempsw) <- paste("t", hourdf$X1, hourdf$X2, sep = "")
                tempsp <- merge(buttonID[1:3], tempsw, by.x = "IBUTTON_ID", by.y = "row.names")
               
rm(tempsw, humdsw, hourdf)

# The coordinates will be loaded from the humdsp and tempsp dataframes and projected in the WGS84 projection.

coordinates(humdsp) <- ~X+Y
        proj4string(humdsp) <- CRS("+init=epsg:4326")
coordinates(tempsp) <- ~X+Y
        proj4string(tempsp) <- CRS("+init=epsg:4326")
       
# The spatial point dataframes can be written to the workspace in different formats (KML and ESRI shapefiles).

writeOGR(humdsp, dsn="IBhumd.kml", layer="ibuttons", driver="KML", overwrite_layer=TRUE)
writeOGR(tempsp, dsn="IBtemp.kml", layer="ibuttons", driver="KML", overwrite_layer=TRUE)
        #writeOGR(humdsp, file, layer="humdsp", driver="ESRI Shapefile", overwrite_layer=TRUE)
        #writeOGR(tempsp, file, layer="tempsp", driver="ESRI Shapefile", overwrite_layer=TRUE)

# The code below will load the KML (or other shapefile) that has to be created ot the relevant study area.
# The study area and coordinates will be converted in the WGS84 and a local projection (has to be changed).
# A NA CRS will have to be set for the study area as well, as this will be useful for the interpolation.

studyarea <- readOGR("area.kml", "area")                                # This file can be in other formats
        proj4string(studyarea) <- CRS(as.character(NA))
                area_for_interpolation <- studyarea
        proj4string(studyarea) <- CRS("+init=epsg:4326")
                area <- spTransform(studyarea, CRS("+init=epsg:2197"))  # This will depend on the region
coords <- as.data.frame(buttonID) 
        coordinates(coords) <- ~X+Y
                proj4string(coords) <- CRS("+init=epsg:4326")
                        localcoords <- spTransform(coords, CRS("+init=epsg:2197"))

# Using the OpenStreetMap Package you can create a background layer to show the location of the study area. 
# You can get the coordinates from http://www.openstreetmap.org/ (enter in from top, left, bottom, to right).

type <- c("osm", "osm-bw", "maptoolkit-topo", "mapquest-aerial", "esri-topo", "apple-iphoto") 
map  <- openmap(c(55.7156, 12.4992), c(55.6946, 12.5636), type = type[3])

        studyregion <- spTransform(studyarea, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0
                        +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))       
        observations <- spTransform(coords, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0
                        +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
        plot(map, raster = TRUE)
        plot(studyregion, add = TRUE, border = "darkblue", col = NA)
        plot(observations, add = TRUE, col = "darkred", pch = 7)

# This block will provide basic information about the study area and the placement of the sensors in the area.
# Information include area size, relation between sensors and  a check whether the sensors are placed correct.

area_km2 <- gArea(area)/1000000 
        pdist_m <- (spDists(coords, longlat=TRUE))*1000
                minpdist_m <- min(pdist_m[pdist_m > 0])
                maxpdist_m <- max(pdist_m)
        border <- as(area, "SpatialLines")
                min_dist_border_to_button_m <- gDistance(border, localcoords)     
                BorderCorrect = if(min_dist_border_to_button_m < minpdist_m) {TRUE} else {FALSE} 

# This block will get the distance to the nearest coast, which will be relevant to understand the local climate. 
# More relevant local factors can be added ttha might have a larger impact on the different sensors in the study.

coast <- getData('GADM', country='DNK',level=0) ## getData("ISO3") can be used to get the country code
        coastline <- spTransform((as(coast, "SpatialLines")), CRS("+init=epsg:2197"))
                min_dist_to_coast_km <- gDistance(localcoords, coastline)/1000  
TopoInfo <- data.frame(area_km2, BorderCorrect, minpdist_m, maxpdist_m,  
                min_dist_border_to_button_m, min_dist_to_coast_km)

rm(list=ls(pattern="dist"), area_km2, coast, coastline, border, localcoords, area,
          BorderCorrect, SensorsCorrect, datumdf, type, studyregion, observations)

# The kriging will be based on the 'fields' package, for which the data will be best prepared in lists.
# The lists will consists of four matrices: lon.lat (2 columns), z (meter), t (Celsius) and h (% RH).
# In a final list (interpolation) the four matrices will be combined and renamed to the names above.

        x <- as.matrix(data.frame(buttonID$X, buttonID$Y))
                colnames(x) <- c("X", "Y")
        z <- as.matrix(data.frame(buttonID$Z))
        t <- as.matrix(colMeans(templog), as.numeric)
        h <- as.matrix(colMeans(humdlog), as.numeric)

interpolation <- list(lonlat, z, t, h)
                names(interpolation) <- c("x", "z", "t", "h")

# Additional kriging information (this section will receive a thorough revision) with several variables.

        d <- ncol(x)
        m <- max(c(2, ceiling(d/2 + 0.1)))
        cov.function <-
                "Thin plate spline radial basis functions (RadialBasis.cov) using great circle distance "
        Krig(x=x, Y=t, cov.function = stationary.cov, m = m, scale.type = "range",
                GCV = TRUE, cov.args=list(Covariance="RadialBasis",M=m, dimension=2, Distance="rdist.earth",
                Dist.args=list(miles=TRUE)) )

# Kriging will be based on the fields package, for which a large number of parameters can be adjusted.
# The help-file is available at http://www.image.ucar.edu/GSP/Software/Fields/Krig.shtml for details.

height <- Krig(interpolation$x, interpolation$z) 
temprt <- Krig(interpolation$x, interpolation$t) 
humdit <- Krig(interpolation$x, interpolation$h) 
        
# The final part of this basis script to log and map iButtons will show a basic raster of the area.
# In addition to this, the three interpolated variables will also be shown to give a first overview.

surface(height, type="p") # look at the surface height
surface(temprt, type="C") # look at the surface temperature
surface(humdit, type="C") # look at the surface humidity

