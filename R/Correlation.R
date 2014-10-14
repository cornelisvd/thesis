## covariates
    source(Aggregate.R)
    setwd ("~/shadowmap/") 
    library(animation)
    library(insol)
    library(raster)
    library(rgdal)
    library(rgeos)
    library(solaR)    

    meantemp[1] <- NULL # from list.mean
    mintemp[1] <- NULL
    maxtemp[1] <- NULL
    meanhumd[1] <- NULL # from list.mean
    minhumd[1] <- NULL
    maxhumd[1] <- NULL
    
    z <- as.data.frame(l) ## spatialaggregate function
    LAI  <- raster("/home/kees/thesis/data/GIS/LAI_new_UTM17N_2m_geotif.tif") 
    veg <- extract(LAI, pts, buffer = 25, fun = mean)
    lidara1 <- read.table("/home/kees/thesis/data/GIS/turrialba.lce.txt")
    lidara2 <- read.table("/home/kees/thesis/data/GIS/turrialba_lce.txt")
    proj  = "+proj=longlat+datum=WGS84"
    lidara1[1] <- lidara1[1] - 360
    lidara2[1] <- lidara2[1] - 360
    names(lidara1) <- c("x", "y", "z")
    names(lidara2) <- c("x", "y", "z")
    e <- extent(lidara1[,1:2])
    r <- raster(e, ncol = 1000, nrow = 1000) 
    lidar1 <- rasterize(lidara1[,1:2], r, lidara1[,3],  fun = mean)
    lidar2 <- rasterize(lidara2[,1:2], r, lidara2[,3],  fun = mean)
    proj4string(lidar1) <- CRS("+proj=longlat +datum=WGS84")
    proj4string(lidar2) <- CRS("+proj=longlat +datum=WGS84")
    lidar1 <- crop(lidar1, poly)
    lidar2 <- crop(lidar2, poly)
    lidar1 <- mask(lidar1, poly)
    lidar2 <- mask(lidar2, poly)
    lidar1 <- disaggregate(lidar1, fact = 10, method = "bilinear")
    lidar2 <- disaggregate(lidar2, fact = 10, method = "bilinear")
    height <- lidar2 - lidar1
    plotvegh <- extract(height, pts, buffer = 25, fun = mean)
    slopep <- extract(slope, pts, buffer = 25, fun = mean)
    aspect <- extract(aspect, pts, buffer = 25, fun = mean)
    insol <- extract(s2, pts, buffer = 25, fun = mean)
    alt2 <- extract(dem, pts, buffer = 25, fun = mean)
    

alt <- as.numeric(l$Z)
    alt <- alt/1000

cor.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), alt2)
        cor.meanT <- append(cor.meanT, c)
    }

cor.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), alt2)
        cor.minT <- append(cor.minT, c)
}

cor.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), alt2)
        cor.maxT <- append(cor.maxT, c)
}
  
veg.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), veg)
        veg.meanT <- append(veg.meanT, c)
}
    
veg.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), veg)
        veg.minT <- append(veg.minT, c)
}   
    
veg.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), veg)
        veg.maxT <- append(veg.maxT, c)
    }
    
    vegh.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), vegh, use = "complete.obs")
        vegh.meanT <- append(vegh.meanT, c)
    }
    
    vegh.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), vegh, use = "complete.obs")
        vegh.minT <- append(vegh.minT, c)
    }   
    
    vegh.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), vegh, use = "complete.obs")
        vegh.maxT <- append(vegh.maxT, c)
    }
    
    hs <- c()
    for (n in 1:nlayers(sunstack)){
        h <- extract(sunstack[[n]], pts, buffer = 25, fun = mean)
        hs <- rbind(hs, h)
    }
    
    ds <- c()
    for (n in 1:nlayers(s)){
        d <- extract(s[[n]], pts, buffer = 25, fun = mean)
        ds <- rbind(ds, d)
    }
    
  plot
    sun2 <- t(apply(ds, 1, cumsum)) 
    
    
    
    cs <- c()
    for (n in 1:nlayers(sunstack)){
        d <- extract(sunstack[[n]], pts, buffer = 50, fun = mean)
        cs <- rbind(cs, d)
    }
    
    hs.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), ds[i,], use = "complete.obs")
        hs.meanT <- append(hs.meanT, c)
    }
    
    sun2 = apply(ds, 2, cumsum)
    
    insC.meanT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), sun2[i,], use = "complete.obs")
        insC.meanT <- append(insC.meanT, c)
    }
    
    hs.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), sun2[i,], use = "complete.obs")
        hs.maxT <- append(hs.maxT, c)
    }
    
    ds.meanT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(meantemp[i,]), ds[i,], use = "complete.obs")
        ds.meanT <- append(ds.meanT, c)
    }
    
    ds.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), ds[i,], use = "complete.obs")
        ds.minT <- append(ds.minT, c)
    }
    
    ds.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), alt, use = "complete.obs")
        ds.maxT <- append(ds.maxT, c)
    }
    
    cs.meanT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(meantemp[i,]), sun, use = "complete.obs")
        cs.meanT <- append(cs.meanT, c)
    }
    
    ins.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), ins, use = "complete.obs")
        ins.meanT <- append(ins.meanT, c)
    }
    
    cs.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), sun, use = "complete.obs")
        cs.maxT <- append(cs.maxT, c)
    }
    
    newrow <- c(0)
    cs <- rbind(newrow,cs)

    slop.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), slopep, use = "complete.obs")
        slop.meanT <- append(slop.meanT, c)
    }
    
    slop.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), slopep, use = "complete.obs")
        slop.minT <- append(slop.minT, c)
    }   
    
    slop.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), slopep, use = "complete.obs")
        slop.maxT <- append(slop.maxT, c)
    }
    
    asp.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), insol, use = "complete.obs")
        asp.meanT <- append(asp.meanT, c)
    }
    
    asp.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), insol, use = "complete.obs")
        asp.minT <- append(asp.minT, c)
    }   
    
    asp.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), insol, use = "complete.obs")
        asp.maxT <- append(asp.maxT, c)
    }
    
  