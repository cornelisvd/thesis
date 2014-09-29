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

alt <- as.numeric(z$Z)

cor.meanT <- c()
    for (i in 1:nrow(meantemp)) {
        c <- cor(as.numeric(meantemp[i,]), alt)
        cor.meanT <- append(cor.meanT, c)
    }

cor.minT <- c()
    for (i in 1:nrow(mintemp)) {
        c <- cor(as.numeric(mintemp[i,]), alt)
        cor.minT <- append(cor.minT, c)
}

cor.maxT <- c()
    for (i in 1:nrow(maxtemp)) {
        c <- cor(as.numeric(maxtemp[i,]), alt)
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