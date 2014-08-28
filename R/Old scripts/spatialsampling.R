## This script provides different options of spatial sampling in a *.kml polygon
## The default method is to create different strata {spcosa} in which a single
## sample will be done. Other types include the methods used in spsample {sp}.

setwd("~/ibuttons/catie") 

sampling <- function(KML, type = "regular", n = 1, s = 1, name = "samples",
                     strata = TRUE, writeKML = TRUE, writeGPS = TRUE) {
    
    require(spcosa)
    require(sp)            
    require(rgdal)
    
    studyarea <- readOGR(paste(KML, ".kml", sep = ""), KML)
    
    if (strata == TRUE) {
            
            mySamplingPattern <- spsample(stratify(studyarea, 
                                 nStrata = n,             
                                 nTry = 10), n = s)        
    } else {
        
            mySamplingPattern <- spsample(studyarea, type = type, n = n)
    }
    
    samples <- as(mySamplingPattern, "SpatialPoints")
    locations <- as.data.frame(samples)
    
    id <- 1:nrow(locations)
    GPS <- cbind(id=id, locations)
    names(GPS) <- c("ID", "x", "y")
    
    coordinates(GPS)= ~x+y
    proj4string(GPS)=CRS("+init=epsg:4326")
      
    if (writeKML == TRUE) {
        writeOGR(GPS, dsn=paste(name, ".kml", sep = ""), layer="", 
        driver="KML", overwrite_layer = FALSE) 
    }
    
    if (writeGPS == TRUE) {
        writeOGR(GPS, dsn=paste(name, ".gpx", sep = ""), layer="waypoints", 
        driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes") 
    }

}



