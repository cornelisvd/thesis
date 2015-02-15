

meansat <- raster("/media/kees/UUI/meanT/tmean8_23.tif")
meansat <- meansat/10
meansat <- projectRaster(meansat, dem)
meansat <- mask(meansat, kml)

meantemp <- calc(temp, mean)
meandiff <- meansat - meantemp


minsat <- raster("/media/kees/UUI/minT/tmin8_23.tif")
minsat <- minsat/10
minsat <- projectRaster(minsat, dem)
minsat <- mask(minsat, kml)

mintemp <- calc(temp, min)
mindiff <- minsat - mintemp

maxsat <- raster("/media/kees/UUI/maxT/tmax8_23.tif")
maxsat <- maxsat/10
maxsat <- projectRaster(maxsat, dem)
maxsat <- mask(maxsat, kml)

maxtemp <- calc(temp, max)
maxdiff <- maxsat - maxtemp
