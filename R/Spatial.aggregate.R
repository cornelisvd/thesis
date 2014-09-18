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
library(gstat)
setwd ("~/thesis/data/")   

# Create spatio-temporal data-frames
agg.spatial <- function(dat = "list.min",
                        dat2 = list.min) 
    {
    
    load(paste(dat, ".Rdata", sep="" ))
    load("sensr.loc.Rdata")
    unit <- c(1,2)
    
    ## Create prediction grid for interpolation
    lc <- na.omit(loc)
    coordinates(lc) = ~X+Y
    proj4string(lc) <- CRS("+proj=longlat + datum=WGS84")
    lc <<- lc
    kml     = readOGR("AquiaresPoly.kml", "AquiaresPoly") 
    r = raster(nrows = 100, ncols = 100)
    extent(r) = extent(kml)
    ras = rasterize(kml, r)
    grt <- as(ras, "SpatialPixels")
    proj4string(grt) = CRS("+proj=longlat +datum=WGS84")
    list.sp <- list()    
    
    for (i in 1:2) {
        
        value <- as.data.frame(dat2[i])
        names <- unlist(strsplit(names(value), "[.]"))
        nm <- names[1:length(names) %% 2 == 0]
        names(value) <- nm
        ### Create spatial points of the useful sensors
        m <- match(nm[2:length(nm)], loc$Number)
        l <- na.omit(loc[m,])
        coordinates(l) = ~X+Y
        proj4string(l) = CRS("+proj=longlat +datum=WGS84")
        pts <- coordinates(l)
        pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
              
        ### Create ST* objects from long/wide tables
        st <- stConstruct(value, space = list(value = 2:ncol(value)), 
                          time = value$Hour, SpatialObj = pts, 
                          interval = TRUE)
    
        ### Create a prediction grid for the spatial interpolaton
        tgrd <- value$Hour
        prd.grd = STF(grt, tgrd)
        
        ### Create a distance to be used in the variogram
        dist <- suppressWarnings(max(gDistance(l, byid=TRUE)*111000))
        
        ### Select the parameters for the actual interpolation
        v <-  vgmST("separable", 
                    space = vgm(1, "Sph", dist), 
                    time  = vgm(1, "Lin", length(value$Hour) * 60), 
                    sill  = 0.1)
        
        ### Create the actual STFDF object and add to list
        stfdf <<- krigeST(values ~ 1, st, prd.grd, v) 
        list.sp[[i]] <- stfdf
    }
    
    ## Add names to the list and include the different units
      list.sp <<- list.sp

    }