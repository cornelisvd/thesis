##---------------------------------Shadow Map---------------------------------##







##---------------------------------v.29-08-14---------------------------------##

# Load libraries & setting the working directory
setwd ("~/shadowmap/") 
library(animation)
library(insol)
library(raster)
library(rgdal)
library(rgeos)
library(solaR)    

# Run scripts from source    
source(".PckgInstall.R")
source(".CastShadows.R")
source(".SunPosition.R")
.pkgInstall()

# Function to create a map of the sun & shadow during a given day
shadow.map <- function(folder    = "data",
                       dem_name  = "dem_cr.tif",       # DEM or raster
                       kml_name  = "Aquiares2.kml",    # KML of the area
                       year      = 2014,               # Year 
                       month     = 8,                  # Month   
                       day       = 22,                 # Day    
                       tmz       = -6,                 # Timezone 
                       horizon   = 10,                 # Buffer for shadows
                       plot.map  = TRUE,               # Plot map in window
                       cr.gif    = TRUE)               # Create an animation
{
    
    ## Download DEMs at http://srtm.csi.cgiar.org/, create KML in Google Earth
    dem     <- raster(paste(folder, "/", dem_name, sep = "")) 
    poly    <- readOGR(paste(folder, "/", "Aquiares.kml", sep = ""), 
                       "Aquiares") 
    border <<- poly
    
    ## Calculate the bounding box and convert to matrix
    lat              <- (bbox(poly)[2,1] + bbox(poly)[2,2]) / 2
    lon              <- (bbox(poly)[1,1] + bbox(poly)[1,2]) / 2
    bb <- as.matrix(bbox(poly))
    
    ## Create the area from which shadows will be calculated            (Note*1)
    bb[1] <- bb[1] - horizon / 111
    bb[2] <- bb[2] - horizon / 111 
    bb[3] <- bb[3] + horizon / 111
    bb[4] <- bb[4] + horizon / 111
    
    ##  Crop DEM by new bounding box and disaggregate by a certain factor
    dem      <-  crop(dem, bb)   
    dem      <-  disaggregate(dem, fact = 10, method = 'bilinear')
    demlong     <-  lon
    slope    <-  terrain(dem, opt='slope')
    aspect   <-  terrain(dem, opt='aspect')
    col      <-  rev(brewer.pal(11,"RdYlBu"))
    dl       <-  daylength(lat,lon,JDymd(year,month,day),tmz)
    
    ## Create a dataframe with the sun position                         (Note*2)
    t <- data.frame()
    for (hour in 7:23) { 
        # for 10 minute-interval uncommemt two lines below
        m <- 0 # c(0, 10, 20, 30, 40, 50) 
        for (min in m) {
            t <- c(t, sunPosition(year, month, day, hour, min, 0, lat, long))
        }
    }
    for (hour in 0:6) { 
        # for 10 minute-interval uncommemt two lines below
        m <- 0 # c(0, 10, 20, 30, 40, 50) 
        for (min in m) {
            t <- c(t, sunPosition(year, month, 23, hour, min, 0, lat, long))
        }
    }
    
    i <- data.frame()
    for (s in 1:length(l)){
        i <-  c(i, insolation(90-50, JDymd(year,month,day), 
                   1100, 25, 95, 300, 0.02, 0.2))
        }
    
    
    ## Create a RasterStacks with the different shadows (hillshade and cast)
    r         <-  raster()
    sunstack  <-  stack()
    l         <-  1 : (length(t)/2)
    x         <-  length(m)   
    u         <-  0
    for (i in 1:length(l)) {
        
        # This shadows range from 0 (shadow) to 1 (sun)               (Note*3)  
        if (i/x >= floor(dl[1]) && i/x <= ceiling(dl[2])) {
            ds <- .shade(dem, c((90-(t[[i*2-1]])), t[[i*2]]), sombra = dem)
            hs <- hillShade(slope, aspect, 90-t[[i*2-1]], t[[i*2]])
            dc <- crop(ds, poly)
            dm <- mask(dc, poly)
            hc <- crop(hs, poly)
            hm <- mask(hc, poly)
            rs <- (dm + 2/3*(hm + 0.5))/2
            u <- u + 1
            ### If there is no daylight at the time, shadows will not be calculated     
        } else {
            r  <- setValues(dem, 0)
            r  <- crop(r, poly)
            rs <- mask(r, poly)
        }
        sunstack <- addLayer(sunstack, rs)
    }
    
    ## A stack will be created with rasters for all the selected times
    names(sunstack) <- paste("time: ", l, ":00", sep="")
    sunstack <<- sunstack
    
    ## This will create an *.gif animation in the *** ColorRamp
    ## Download ImageMagick from http://www.imagemagick.org/script/download.php
    if (cr.gif == TRUE) {
        cols = colorRampPalette(c("red","white"),bias=.1,space="rgb")
        saveGIF(
            print(spplot(sunstack, movie.name = "ani.gif", height = 500, width = 350,
                         layout=c(1, 1), col.regions = cols, interval = .1, outdir = getwd()) + 
                      layer(sp.polygons(border))))
    }
    
    ## This will plot a map in the default R window of the average values
    if (plot.map == TRUE) {
        sunmap <- calc(sunstack, mean)
        return(plot(sunmap))
    }
}

##------------------------------------Notes-----------------------------------##

cor(list.ras$temperature@layers$ , list.ras$humidity
)
