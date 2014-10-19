##----------------------------STFDF to RasterBrick----------------------------##
## This function converts STFDF objects ({spacetime} - spatio-temporal data with 
## full space-time grid) to RasterBrick objects for calculation & visualization.
## The resulting RasterBrick object is easily animated with the plotKML package.
##--------------------------------v.19-09-14----------------------------------##

# Load libraries
    library(raster)
    library(spacetime)
    library(plotKML)
# Load STFDF for example
    setwd ("~/thesis/data/")
    load("tempSTFDF.Rdata")
    stfdf <- list.sp[[1]]

# Function to create raster from the STFDF file
    create.rast <- function (obj = stfdf,                 ## The STFDF object
                          brickx = TRUE,                  ## Create RasterBrick
                          lower  = NA,                    ## Lower data limit                            
                          upper  = NA,                    ## Upper data limit
                          tformat = "%Y-%m-%d %H:%M:%S")  ## Could be left out?
    {   
        ## Extracting paramters from the STFDF
        dim <- obj@sp@grid@cells.dim 
        ex  <- extent(obj@sp@bbox)
        n   <- length(obj@time)
        r   <- raster(ex, nrows = dim[1], ncols = dim[2])
        i   <- obj@sp@grid.index
        v   <- length(obj@data$var1.pred)/n
        a   <- list()
        b   <- seq(from = 1, to = 1 + n*v, by = v)
        e   <- seq(from = v, to = n*v, by = v)
        
        ## Looping the STFDF layers in a list. If this is done directly in a 
        ## stack or brick, it always results in a RasterStack for some reason.
        for (l in 1:n) {
            r[i] <- obj@data$var1.pred[b[l]:e[l]]
            a[[l]] <- r 
        }
        ## Thresholds can be set
        fun <- function(x) { x[x < lower | x > upper] <- NA; return(x) }
                
        ## Create a RasterBrick from the STFDF with the time as z-value
        b <<- brick(a)
        #b <- calc(b, fun)
        #proj4string(b) <- proj4string(obj)
        #t <- rownames(as.data.frame(obj@time))
        #b <- setZ(b, as.POSIXct(t, format = tformat), name = "time")
        #names(b) <- t
        #brickST <<- b
    }
## plot KML as animation in Google Earth    
#kml(brickST, colour_scale = SAGA_pal[[1]], alpha = 0.50, overwrite = TRUE) 
    
