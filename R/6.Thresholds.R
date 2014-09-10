##---------------------------------Thresholds---------------------------------##
## This R-script will analyse the humidity and/or temperature data in the lists,
## to calculate how often certain thresholds have been reached during the period 
## that has been selected. In order to make a nice visualization, the raster can 
## be disaggregated by a certain factor and be  plotted as KML (in Google Earth).
##---------------------------------v.28-08-14---------------------------------##

# Function to calculate thresholds based on min and max humidity & temperature
    calc.thresh <- function (lowerT   = 5,
                             upperT   = 25,
                             lowerRH  = 30,
                             upperRH  = 110,
                             risk.map = TRUE,
                             dissolve = 10,
                             kml.plot = FALSE) 
{
    ## This section takes the stacks from the lists and calculates thresholds
        t <- list.ras$temperature  
        h <- list.ras$humidity
        r.t <- lowerT  < t | upperT  > t 
        r.h <- lowerRH < h | upperRH > h
    
    ## Here the percentage of time outside limits is calculated and plotted
    r.thres <- calc((r.h + r.t)/0.02, mean)
    thres.da <- disaggregate(r.thres, fact = dissolve, method="bilinear")
    cols <- rev(heat.colors(10))
    if (risk.map == TRUE) {
        return(plot(thres.da, col = cols))
    }
}