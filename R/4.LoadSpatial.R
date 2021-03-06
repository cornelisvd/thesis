##-------------------------------Spatial-sensors------------------------------##
## Function to link dataframes with time & humidity & temperature data to the XY
## coordinates where the sensors have been placed. This creates spatio- temporal
## dataframes (STFDF in the {spacetime} package), and can also be used to create 
## EOF objects (empirical orthogonal function) for Principal Component Analysis.
##---------------------------------v.27-08-14---------------------------------##

# Load libraries
library(sp)     
library(splancs)
library(spacetime)
library(rgeos)
library(rgdal)
library(raster)
library(gstat)
library(plyr)

# Create spatio-temporal data-frames
load.spatial <- function(cells = 1000,                        # Nr. of cells    
                         start = "2014-08-07",                # Start season
                         end   = "2014-08-08",                # Season end                   
                         plot  = FALSE,                       # Plot yes/no
                         proj  = "+proj=longlat+datum=WGS84") # Projection
    ## plotKML option
{
    
    ## Select humidity and/or temperature
    ubol <- c(temperature, humidity)
    unit <- which(ubol == TRUE)
    
    ## Create length of the spatial object
    tm <- as.integer(difftime(end, start, units='hours'))
    
    ## Create prediction grid for interpolation
    lc <- na.omit(loc)
    coordinates(lc) = ~X+Y
    proj4string(lc) <- CRS("+proj=longlat +datum=WGS84")
    lc <<- lc
    
    ## Create a convex hull around the sensors
    ch <- chull(lc$X, lc$Y)
    ch <- c(ch, ch[1])
    border <- cbind(lc$X[ch], lc$Y[ch]) 
    grd <- as.data.frame(gridpts(border, cells)) 
    gridded(grd) = ~V1+V2
    proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
    list.sp <- list()    
    
    ## Create the spatio-temporal data-frames
    for (k in unit) {
        obs.cor <- as.data.frame(list.cor[k])
        colnames(obs.cor)[1] <- "date"
        
        ### Make a subset of the data for the desired period
        obs.cor <- subset(obs.cor, obs.cor$date > as.POSIXct("2014-08-07") & 
                              obs.cor$date <= as.POSIXct("2014-08-08"))
        dates <- obs.cor[1]
        obs.cor[1] <- NULL
        names <- unlist(strsplit(names(obs.cor), "X"))
        nm <- names[1:length(names) %% 2 == 0]
        
        ### Create spatial points of the useful sensors
        m <- match(nm, loc$Number)
        l <- na.omit(loc[m,])
        l <- arrange(l,iButton.ID)
        coordinates(l) = ~X+Y
        proj4string(l) = CRS("+proj=longlat +datum=WGS84")
        pts <- coordinates(l)
        pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
        
        kml     <- readOGR("AquiaresPoly.kml", "AquiaresPoly") 
        dem     <- raster("srtm_20_11.tif") 
        dem     <- crop(dem, kml)
        #extent(r) = extent(kml)
        #ras = rasterize(kml, r)
        grt <- as(dem, "SpatialPixels")
        proj4string(grt) <- CRS("+proj=longlat +datum=WGS84")
        
        ### Create ST* objects from long/wide tables
        st <- stConstruct(obs.cor, space = list(values = 1:ncol(obs.cor)), 
                          time = dates$date, SpatialObj = pts, interval = TRUE)
        
        ### Create a prediction grid for the spatial interpolaton
        tgrd <- seq(min(index(st)), max(index(st)), length = tm)
        prd.grd = STF(grt, tgrd)
        
        ### Create a distance to be used in the variogram
        dist <- suppressWarnings(max(gDistance(l, byid=TRUE)*111000))
        
        ### Select the parameters for the actual interpolation
        v <-  vgmST("separable", 
                    space = vgm(1, "Exp", 100), 
                    time  = vgm(1, "Lin", 24 * 60), 
                    sill  = 1)
        
        ### Create the actual STFDF object and add to list
        stfdf <<- krigeST(values ~ 1, st, prd.grd, v) 
        list.sp[[k]] <- stfdf
    }
    
    ## Add names to the list and include the different units
    names(list.sp) <- units
    list.sp <<- list.sp
    
    ## Plot the STFDF object if plot is set to 'TRUE'
    if (plot == TRUE) {
        return(stplot(stfdf, cuts = 10, col.regions = brewer.pal(10, "RdBu"))) 
        
        ## Animate as KML option    
        # if plotKML = TRUE
        #
    }
}