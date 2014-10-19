library(sp)     
library(splancs)
library(spacetime)
library(rgeos)
library(rgdal)
library(gstat)

    load("list.cor.Rdata")
    lc <- na.omit(loc)
    coordinates(lc) = ~X+Y
    proj4string(lc) <- CRS("+proj=longlat + datum=WGS84")
    lc <<- lc
    kml     = readOGR("AquiaresPoly.kml", "AquiaresPoly") 
    r = raster(nrows = 50, ncols = 50)
    extent(r) = extent(kml)
    ras = rasterize(kml, r)
    grt <- as(ras, "SpatialPixels")
    proj4string(grt) = CRS("+proj=longlat +datum=WGS84")

    m <- match(names(temp[2:length(temp)]), loc$Number)
    l <- na.omit(loc[m,])
    coordinates(l) = ~X+Y
    proj4string(l) = CRS("+proj=longlat +datum=WGS84")
    pts <- coordinates(l)
    pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
    ### Create a distance to be used in the variogram
    dist <- suppressWarnings(max(gDistance(l, byid=TRUE))*111000)


t.cor <- list.cor[[2]]
season <- seq(as.Date("2014-08-07"), as.Date("2014-09-07"), by = 1)
list.stfdf <- list()


    ptm <- proc.time()
    
    for (i in 1:length(season)){
        temp   <- subset(t.cor, t.cor$Date > as.POSIXct(as.Date(season[i])) &
                               t.cor$Date <= as.POSIXct(as.Date(season[i+1])))
        st <- stConstruct(temp, space = list(value = 2:ncol(temp)), 
                          time = temp$Date, SpatialObj = pts, 
                          interval = TRUE)
        tgrd <- temp$Date
        prd.grd = STF(grt, tgrd)
        ### Select the parameters for the actual interpolation
        v <-  vgmST("separable", 
                    space = vgm(1, "Sph", dist), 
                    time  = vgm(1, "Lin", 3), 
                    sill  = 1)
        stfdf <<- krigeST(formula=values~1, data=st, prd.grd, v) 
        create.rast()
        list.stfdf[[i]] <- b  
    }
    
    brickt <- brick(list.stfdf)
    proj4string(brickt) <- CRS("+proj=longlat + datum=WGS84")

    
    proc.time() - ptm

ptm <- proc.time()
load.sensors()
proc.time() - ptm
    
