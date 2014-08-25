### Function to convert STFDF (spacetime package) objects to a RasterStack


STFDF2Raster <- function(x = stfdf) {
           require(raster)
           require(spacetime)
           
    dim <- x@sp@grid@cells.dim 
        ex <- extent(x@sp@bbox)
            n <- length(x@time)
                r <- raster(ex, nrows = dim[1], ncols = dim[2])
                    i <- x@sp@grid.index
                        v <- length(x@data$var1.pred)/n
                            s <- stack(r)
                                b <- seq(from = 1, to = 1 + n*v, by = v)
                                    e <- seq(from = v, to = n*v, by = v)
    
    for (l in 1:n) {
                     r[i] <- x@data$var1.pred[b[l]:e[l]]
                     s <- addLayer(s, r) 
    }
    
    names(s) <- x@endTime
    rs <<- s
    return(animate(s, pause = 0.5, n = 1, col = rainbow))
    
    
}