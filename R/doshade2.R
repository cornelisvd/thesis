shade <- function (dem, sv, dl = 0, sombra = dem) 
{

    if (!is.loaded("doshade")) {
        dyn.load("doshade.so")
    }
    if (nargs() < 2) {
        cat("USAGE: doshade(dem,sunvector,dl) \n")
        return()
    }
    switchdem = 0
    if (class(dem) == "RasterLayer") {
        switchdem = 1
        dproj = projection(dem)
        dext = extent(dem)
        if(!isLonLat(dem)){dl = res(dem)[1]} else{
            y <- round(nrow(dem)/2)
            y1 <- yFromRow(dem, y)
            y2 <- yFromRow(dem, y+1)
            x1 <- xFromCol(dem, 1)
            x2 <- xFromCol(dem, 2)
            dl = pointDistance(c(x1,y1), c(x2,y2), lonlat=TRUE)        
        }
        dem = raster::as.matrix(dem)
    }
    cols = ncol(dem)
    rows = nrow(dem)
    if (dl == 0) {
        cat("Input data is not a RasterLayer, then I need the DEM resolution dl \n")
        return()
    }
    dem[is.na(dem)] = -999
    out = .Fortran("doshade", dem = as.numeric(t(dem)), sunvector = as.vector(sv), 
                   cols = as.integer(cols), rows = as.integer(rows), dl = as.double(dl), 
                   sombra = as.numeric(dem), PACKAGE = "insol")
    sombra = t(matrix(out$sombra, nrow = cols))
    if (switchdem) {
        sombra = raster(sombra, crs = dproj)
        extent(sombra) = dext
    }
    sombra[sombra == -999] = NA
    return(sombra)
}