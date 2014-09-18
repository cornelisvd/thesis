##------------------------------STFDF to Raster-------------------------------##
## This function converts STFDF objects ({spacetime} - spatio-temporal data with 
## full space-time grid) to RasterStack or RasterBrick objects for calculations.
## This version is adjusted for the iButtons, a default version is also created.
##--------------------------------v.26-08-14----------------------------------##

# Load libraries
library(raster)
setwd ("~/thesis/data/")   
load("mean.sp.Rdata")

# Function to create raster from the STFDF file
create.rast <- function (x = stfdf) 
{   
    
    # This can be used to create multiple RasterStacks for different units       
    # ubol <- c(temperature, humidity)
    # unit <- which(ubol == TRUE)
    unit <- c(1,2)
    list.ras <- list() 
    
    for (j in unit) {
        x <- unlist(list.sp[[j]])
        dim <- x@sp@grid@cells.dim 
        ex <- extent(x@sp@bbox)
        n <- length(x@time)
        r <- raster(ex, nrows = dim[1], ncols = dim[2])
        i <- x@sp@grid.index
        v <- length(x@data$var1.pred)/n
        s <- list()
        b <- seq(from = 1, to = 1 + n*v, by = v)
        e <- seq(from = v, to = n*v, by = v)
        
        for (l in 1:n) {
            r[i] <- x@data$var1.pred[b[l]:e[l]]
           
            s[[l]] <- r 
        }
        names(s) <- x@endTime
        
        list.ras[[j]] <- s
    }

    list.ras <<- list.ras
      
}  

x22 <- brick(list.ras[[1]])
psx <- rownames(as.data.frame(x@time))
x22 <- setZ(x22, as.POSIXct(psx), name='time')
proj4string(x22) <- CRS("+proj=longlat +datum=WGS84")
kml(x22, colour_scale=SAGA_pal[[1]])
