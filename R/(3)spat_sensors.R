### Function to create a grid from the registration files of the micro-sensors
spat.sensors <- function(reg = "/home/kees/ibuttons/Honduras/ibuttons.csv",
                cells = 1000, period = "2013-06-25", n = 24, saveKML = TRUE) {
    
                    require(sp)     
                    require(splancs)
                    require(spacetime)
                    require(RColorBrewer)
                    require(gstat)
            
    loc  <- read.csv(reg) 
        m <- match(names(obs.ok), loc$CODE) 
        loc <- na.omit(loc[m,])
            coordinates(loc) = ~X+Y
                proj4string(loc) = "+proj=longlat +datum=WGS84"
                      pts <- coordinates(loc)
                            obs.cor <- obs.ok[2:ncol(obs.ok)]

    pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
        ch <- chull(loc$X, loc$Y)
            ch <- c(ch, ch[1])
                border <- cbind(loc$X[ch], loc$Y[ch]) 
                    grd <- as.data.frame(gridpts(border, cells)) 
                        gridded(grd) = ~V1+V2
                            proj4string(grd) <- proj4string(loc)
  
        st <- stConstruct(obs.cor, space = list(values = 1:ncol(obs.cor)), 
                    time = obs.ok$Date, SpatialObj = pts, interval = TRUE)
            st <- st[, period] 
                  tgrd <- seq(min(index(st)), max(index(st)), length=n)
                            prd.grd = STF(grd, tgrd)
                                v <-  vgmST("separable", 
                                    space = vgm(1, "Sph", 30000), 
                                        time  = vgm(1, "Lin", n * 60), 
                                            sill  = 0.1)
    
    stfdf <<- krigeST(values ~ 1, st, prd.grd, v)
        return(stplot(stfdf, cuts = 10, col.regions = brewer.pal(10, "RdBu"))) 
           
} 