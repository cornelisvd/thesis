##--------------------------------Plot-sensors--------------------------------##
## Function to create plots for all the micro-sensors that have been used in the
## project. This is the second function in which thresholds & limits can be set,
## while additional information will be added from the 'loc' file for this area. 
## A new folder will be created in the working directory for all of these plots.
##---------------------------------v.26-08-14---------------------------------##

# Load libraries
    library(sp)     
    library(splancs)
    library(spacetime)
    library(rgeos)
    library(gstat)

# Create spatio-temporal data-frames
    load.spatial <- function (cells = 1000,                       # Nr. of cells    
                              start = "2013-06-23",               # Season start
                              end   = "2013-06-30",               # Season end                   
                              plot  = FALSE)                      # Plot yes/no
                              proj  = "+proj=longlat+datum=WGS84" # Projection
    {
       
    # Select humidity and/or temperature
    ubol <- c(temperature, humidity)
    unit <- which(ubol == TRUE)
    
    # Create length of the spatial object
    tm <- as.integer(difftime(end, start, units='hours'))
    
    # Create prediction grid for interpolation
    lc <- na.omit(loc)
    coordinates(lc) = ~X+Y
    proj4string(lc) <- proj
    lc <<- lc
    
    #
    ch <- chull(lc$X, lc$Y)
    ch <- c(ch, ch[1])
    border <- cbind(lc$X[ch], lc$Y[ch]) 
    grd <- as.data.frame(gridpts(border, cells)) 
    gridded(grd) = ~V1+V2
    proj4string(grd) <- proj
    list.sp <- list()    
    
    # Create the spatio-temporal data-frames
    for (k in unit) {
        obs.cor <- as.data.frame(list.cor[k])
        colnames(obs.cor)[1] <- "date"
        
        #
        obs.cor <- subset(obs.cor, obs.cor$date >= as.POSIXct(start)
                          & obs.cor$date <= as.POSIXct(end))
        dates <- obs.cor[1]
        obs.cor[1] <- NULL
        names <- unlist(strsplit(names(obs.cor), "[.]"))
        
        #    
        m <- match(names[1:length(names)], loc$CODE) 
        l <- na.omit(loc[m,])
        coordinates(l) = ~X+Y
        proj4string(l) = proj
        pts <- coordinates(l)
        pts <- SpatialPoints(pts, CRS(proj))
        
        #
        st <- stConstruct(obs.cor, space = list(values = 1:ncol(obs.cor)), 
                          time = dates$date, SpatialObj = pts, interval = TRUE)
        tgrd <- seq(min(index(st)), max(index(st)), length = tm + 1)
        prd.grd = STF(grd, tgrd)
        dist <- suppressWarnings(max(gDistance(l, byid=TRUE)*111000))
        
        # Select the parameters for the actual interpolation
        v <-  vgmST("separable", 
                    space = vgm(1, "Sph", dist), 
                    time  = vgm(1, "Lin", tm * 60), 
                    sill  = 0.1)
        stfdf <<- krigeST(values ~ 1, st, prd.grd, v) 
        list.sp[[k]] <- stfdf
    }

    #
    names(list.sp) <- units
    list.sp <<- list.sp
    
    #
    if (plot == TRUE) {
        return(stplot(stfdf, cuts = 10, col.regions = brewer.pal(10, "RdBu"))) 
    }
    
}