##---------------------------------Shadow Map---------------------------------##






##---------------------------------v.20-08-14---------------------------------##

# Load libraries & setting the working directory
    setwd ("~/thesis/data/gis") 
    library(raster)
    library(solaR)
    library(rgdal)
    library(rgeos)
    library(insol)
    
# Function to create a map of the sun & shadow during a given day
    sun.map  <-  function(dem_name = "srtm_20_11.tif", 
                          kml_name = "Aquiares2.kml", 
                          year     = 2014, 
                          month    = 8,  
                          day      = 24,
                          tmz      = -6,
                          horizon  = 10,
                          plot.map = TRUE,
                          cr.gif   = TRUE,
                          weight   = 1) 
                 {

        
    ##
        dem     = raster(dem_name) 
        kml     = readOGR(kml_name, kml_name) 
        
    ## 
        lat              <- (bbox(kml)[2,1] + bbox(kml)[2,2]) / 2
        lon              <- (bbox(kml)[1,1] + bbox(kml)[1,2]) / 2
    
    ##
        bb <- as.matrix(bbox(kml))
            bb[1] <- bb[1] - horizon / 111
            bb[2] <- bb[2] - horizon / 111 
            bb[3] <- bb[3] + horizon / 111
            bb[4] <- bb[4] + horizon / 111
        
    ##    
        dem              <- crop(dem, bb)   
        dem <- disaggregate(dem, fact=10, method='bilinear')
        long             <- lon
        slope            <- terrain(dem, opt='slope')
        aspect           <- terrain(dem, opt='aspect')
        col              <- rev(brewer.pal(11,"RdYlBu"))
        dl               <- daylength(lat,lon,JDymd(year,month,day),tmz)
    
    ##
        t <- data.frame()
            for (i in 0:23)               {
                t <- c(t, sunpos(year, month, day, i, 0, 0, lat, long))
        }
    
    ##   
        r <- raster()
        s <- stack()
        l <- 1 : (length(t)/2)
    
    ##
        for (i in 1:length(l)) {
        ###    
            if (i >= floor(dl[1]) && i <= ceiling(dl[2])) {
                ds <- shade(dem, c((90-(t[[i*2-1]])), t[[i*2]]), sombra = dem)
                hs <- hillShade(slope, aspect, 90-t[[i*2-1]], t[[i*2]])
                dc <- crop(ds, kml)
                dm <- mask(dc, kml)
                hc <- crop(hs, kml)
                hm <- mask(hc, kml)
                rs <- (weight*dm+0.667*(hm+0.5))/(weight+1)
        ###     
            } else {
                r <- setValues(dem, 0)
                r <- crop(r, kml)
                rs <- mask(r, kml)
            }
                s <- addLayer(s, rs)
    }

    ##
        names(s) <- paste("time: ", l, ":00", sep="")
        sunstack <<- s
        sun <- calc(s, sum)/dl[3]
    
    if (plot.map == TRUE) {
        return(plot(sun))
    }
}