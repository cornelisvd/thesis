library(raster)
library(GSIF)
library(randomForest)
library(fossil)
library(gstat)
library(rgdal)
library(spacetime)
library(automap)
library(rgeos)
library(ggplot2)

setwd ("~/thesis/data/")  
load("TempCor.Rdata")
load("pts.Rdata")
load("dem.Rdata")
load("slope.Rdata")
load("aspect.Rdata")
load("sun.Rdata")
load("kml.Rdata")
load("height.Rdata")
vegetation  <- raster("/home/kees/thesis/data/GIS/LAI_new_UTM17N_2m_geotif.tif") 
ws <- readOGR("Watershed.kml", "Watershed")

solx <- c()
for (l in 1:length(sun@data@names)){
    solx <- append(solx, extract(sun[[l]], pts, buffer = 10, fun = mean))
}
solx <- t(matrix(solx, nrow=80))

area  <- crop(dem, kml)
pts2  <- SpatialPoints(coordinates(area))
pts1  <- SpatialPointsDataFrame(as.data.frame(pts2), data=as.data.frame(rep(
          1,nrow(as.data.frame(pts2)))))
projection(pts1) <- projection(dem)
nona  <- na.exclude(as.data.frame(pts1))
nona  <- nona[,-4]
coordinates(nona) <- ~x+y
gridded(nona) <- TRUE
projection(nona) <- projection(kml)
radiation <- calc(sun, sum)

demgrd <- as(dem, "SpatialGridDataFrame")
slpgrd <- as(slope, "SpatialGridDataFrame")
aspgrd <- as(aspect, "SpatialGridDataFrame")
radgrd <- as(radiation, "SpatialGridDataFrame")
veggrd <- as(vegetation, "SpatialGridDataFrame")
cangrd <- as(height, "SpatialGridDataFrame")
    projection(demgrd) <- projection(kml)
    projection(slpgrd) <- projection(kml)
    projection(aspgrd) <- projection(kml)
    projection(radgrd) <- projection(kml)
    projection(veggrd) <- projection(kml)
    projection(cangrd) <- projection(kml)
        overdem <- over(nona, demgrd)
        overslp <- over(nona, slpgrd)
        overasp <- over(nona, aspgrd)
        overrad <- over(nona, radgrd)
        overveg <- over(nona, veggrd)
        overcan <- over(nona, cangrd)
            nona$altitude  <- overdem$dem_cr
            nona$slope     <- overslp$slope
            nona$aspect    <- overasp$aspect
            nona$radiation <- overrad$layer
            nona$vegetation <- overveg$LAI_new_UTM17N_2m_geotif
            nona$canopy     <- overcan$layer

d <- 31
temp    <- t(temp.cor[1:(24*d),2:81])
data.f  <- as.data.frame(pts)
data.f  <- cbind(data.f, temp)
coordinates(data.f) <- ~x+y
projection(data.f) <- projection(kml)
names(data.f) <- c("x", "y", "temperature")

    dem <- extract(dem, data.f, buffer = 10, fun = mean)
    slp <- extract(slope, data.f, buffer = 10, fun = mean)
    asp <- extract(aspect, data.f, buffer = 10, fun = mean)
    rad <- extract(radiation, data.f, buffer = 10, fun = mean )
    veg <- extract(vegetation, data.f, buffer=10, fun=mean)
    can <- extract(height, data.f, buffer=10, fun=mean)
        data.f$altitude   <- dem
        data.f$slope      <- slp
        data.f$aspect     <- asp
        data.f$radiation  <- rad
        data.f$vegetation <- veg
        data.f$canopy     <- can



newproj <-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 
              +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
data.f <- spTransform(data.f, newproj)
ws <- spTransform(ws, newproj)
kml <- spTransform(kml, newproj)
pts <- spTransform(pts, newproj)
dist <- gDistance(pts, byid=TRUE)

sun <- projectRaster(sun, 
        crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 
        +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
predgrd <- brick(nona)
predgrd <- projectRaster(predgrd, 
crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 
+y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
sun <- as(sun, "SpatialPixelsDataFrame")
predgrd <- as(predgrd, "SpatialPixelsDataFrame")
rad <- over(predgrd, sun)      
rad <- rep(rad, 7)
#solx <- rbind(solx, solx, solx, solx, solx, solx, solx)

day <- 7
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allact <- c()
allprd <- c()
points <- seq(1:80)
    #c(67:74)
ptm <- proc.time()
for (d in 1:day){
    for (i in 1:24){
        n <- (d-1)*24 + i
        temperature <- temp[,n]
        data.f$temperature <- temperature
        data.s  <- data.f[points,]
        #data.c  <- data.f[-points,]
        #IDW1n_fit <- gstat(id="IDW1n_fit", formula = temperature ~ 1, data = data.s,
        #                   set=list(idp=2))
        #pe1 <- gstat.cv(IDW1n_fit, nfold=length(data.s))
        #res <- pe1@data$residual
        #allres <- c(allres, res)
        #RMSE <- c(RMSE, sqrt(mean(res^2)))
## ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~1,#log(altitude), # or 1
        #               data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g, all.residual=TRUE, verbose=FALSE)
        #res <- pe1$temperature
        #allres <- c(allres, res)
        #LOO <- c(LOO, sqrt(mean((res)^2)))

        #res <- extract(x, data.c)
        #res <- (data.c[[i]] - res)^2
        #RMSE <- c(RMSE, sqrt(mean(res)))

## DYNAMIC UNIVERSAL KRIGING
         # if (i <= 5 | i >= 18){
         #       g <- gstat(NULL, id="temperature", form=temperature~log(altitude)+
         #                      log(slope)+log(aspect), # or 1
         #           data=data.s)
         #} else {
         #   predgrd$radiation <- rad[[i]]
         #   radiation <- solx[i,]
         #   data.s$radiation <- radiation[points]
         #   g <- gstat(NULL, id="temperature", form=temperature~log(altitude)+
         #                  log(radiation)+log(aspect)+log(slope), 
         #                  data=data.s)
        #}
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g, all.residual=TRUE, verbose=FALSE)
        #res <- pe1$temperature
        #allres <- c(allres, res)
        #LOO <- c(LOO, sqrt(mean((res)^2)))
        #res <- extract(x, data.c)
        #res <- (data.c[[i]] - res)^2
        #RMSE <- c(RMSE, sqrt(mean(res)))
        
## AUTOMAP KRIGING
        if (i <= 5 | i >= 18){
            kri <- autoKrige(temperature ~ log(altitude) + log(slope) +
                                 log(aspect),
                             data.s, predgrd)
        } else {
            predgrd$radiation <- rad[[i]]
            radiation <- solx[i,]
            data.s$radiation <- radiation[points]
            kri <- autoKrige(temperature ~ log(altitude), + log(radiation) +
                                 log(slope) + log(aspect),
                             data.s, predgrd)
        }    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        kriging.pred <- kri$krige_output
        x <- raster(kriging.pred)
        cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        act <- cv$krige.cv@data$observed
        prd <- cv$krige.cv@data$var1.pred
        allact <- c(allact, act)
        allprd <- c(allprd, prd)
        #res2 <- extract(x, data.c)
        #res3 <- (data.c[[i]] - res2)^2
        #RMSE <- c(RMSE, sqrt(mean(res3)))
              
## FIT.GSTATMODEL
        #radiation <- solx[i,]
        #data.s$radiation <- radiation[points]
        #predgrd$radiation <- rad[[i]]
        #fit <- fit.gstatModel(data.s, temperature~altitude+radiation+
        #                          slope+aspect+vegetation+canopy,
        #                      family = gaussian(log),
        #                      predgrd, 
        #                      method="randomForest")
        #k.2 <- predict(fit, predgrd, nfold=length(data.s))
        #res <- k.2@validation@data$residual
        #LOO <- c(LOO, sqrt(mean(res^2)))
        #x <- raster(k.2@predicted)
    
## CO-KRIGING
    #    predgrd$radiation <- rad[[i]]
    #    radiation <- solx[i,]
    #    data.s$radiation <- radiation[points]
    #g <- gstat(NULL, id="Temperature", form=temperature~1, 
    #                              data=data.s)
    #g <- gstat(g, id="Altitude", form=altitude~1, 
    #                              data=data.s)
    #g <- gstat(g, id="Radiation", form=radiation~1, 
    #       data=data.s)
    #
    #vg <- gstat::variogram(g)
    #vg.fit <- fit.lmc(vg, g, vgm(1, "Sph", 1000))
    #c.k <- predict(vg.fit, predgrd)
    #x <- raster(c.k)
    #x <- x$temperature.pred
    #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))

        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
    TempBrick <- brick(TempList)
proc.time() - ptm

RMSEs <- c()
MAEs <- c()
for (r in 1:nrow(allres)){
    RMSEs <- c(RMSEs, sqrt(mean(allres[r,]^2)))
    MAEs <- c(MAEs, mean(abs(allres[r,])))
}

RMSEt <- c()
MAEt <- c()
for (c in 1:ncol(allres)){
    RMSEt <- c(RMSEt, sqrt(mean(allres[,c]^2)))
    MAEt <- c(MAEt, mean(abs(allres[c,])))
}





temp.cor <- temp.cor[1:24,]
predgrd2 <- as(predgrd, "SpatialPixels")
st <- stConstruct(temp.cor, space = list(temperature = 2:ncol(temp.cor)), 
                  time = temp.cor[, 1], SpatialObj = pts, interval = TRUE)
st@data$altitude <- rep(dem, 744)

control <- stack()
for (d in 1:31){
    e <- d*24
    b <- e-23
    temp <- temp.cor[b:e,]
    st <- stConstruct(temp, space = list(temperature = 2:ncol(temp)), 
                      time = temp[, 1], SpatialObj = pts, interval = TRUE)
    tgrd <- seq(min(index(st)), max(index(st)), length = nrow(temp))
    prd.grd = STF(predgrd, tgrd)
    separableModel <- vgmST("separable",
                            space=vgm(1,"Exp", 250, 0.1),
                            time =vgm(1,"Exp", 3, 0.1),
                            sill=100)
    var <- variogramST(values ~ 1, st, tlags=0:5)
    separableModel <- fit.StVariogram(var, separableModel,
                                      method="L-BFGS-B",
                                      lower=c(10,0,0.01,0,1),
                                      upper=c(500,1,20,1,200))
    stfdf <- krigeST(values ~ 1, st, prd.grd, separableModel) 
    r <- raster(stfdf@sp[1])
    l <- length(stfdf@sp[1])
    for (n in 1:length(stfdf@time)){
        r@data@values[!is.na(r@data@values)] <- 
            stfdf@data$var1.pred[((n*l)-(l-1)):(n*l)]
        control <- addLayer(control, r)
    }
}

orT <- c()
for (p in 1:nlayers(control)) {
    t <- extract(control[[p]], pts)
    orT <- rbind(orT, t)
}
save(control, file="TempBrick_STK.Rdata")
save(orT, file="pred_STK.Rdata")


ST_pred <- c()
for (i in 1:length(pts)){
    krT <- c()
     for (d in 1:7){
    control <- stack()
    e <- d*24
    b <- e-23
    temp <- temp.cor[b:e,]
    st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                      time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
    tgrd <- seq(min(index(st)), max(index(st)), length = nrow(temp))
    prd.grd = STF(predgrd, tgrd)
    st@data$altitude <- rep(dem[-i], 24)
    separableModel <- vgmST("separable",
                            space=vgm(1,"Exp", 250, 0.1),
                            time =vgm(1,"Exp", 3, 0.1),
                            sill=100)
    var <- variogramST(values ~ 1, st)
    separableModel <- fit.StVariogram(var, separableModel,
                                      method="L-BFGS-B",
                                      lower=c(10,0,0.01,0,1),
                                      upper=c(500,1,20,1,200))
    stfdf <- krigeST(values ~ 1, st, prd.grd, separableModel) 
    r <- raster(stfdf@sp[1])
    l <- length(stfdf@sp[1])
    for (n in 1:length(stfdf@time)){
        r@data@values[!is.na(r@data@values)] <- 
            stfdf@data$var1.pred[((n*l)-(l-1)):(n*l)]
        control <- addLayer(control, r)
        }
    for (p in 1:nlayers(control)) {
        t <- extract(control[[p]], pts[i])
        krT <- rbind(krT, t)
        }
    }
    ST_pred <- cbind(ST_pred, krT)
}
save(ST_pred, file="STKE40_allprd.Rdata")

ST_pred <- c()
for (i in c(67:74)){
      krT <- c()
    
    for (d in 1:7){
        control <- stack()
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                          time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
        tgrd <- seq(min(index(st)), max(index(st)), length = nrow(temp))
        prd.grd = STF(predgrd, tgrd)
        separableModel <- vgmST("separable",
                                space=vgm(1,"Exp", 250, 0.1),
                                time =vgm(1,"Exp", 3, 0.1),
                                sill=100)
        var <- variogramST(values ~ 1, st, tlags=0:5)
        separableModel <- fit.StVariogram(var, separableModel,
                                          method="L-BFGS-B",
                                          lower=c(10,0,0.01,0,1),
                                          upper=c(500,1,20,1,200))
        stfdf <- krigeST(values ~ 1, st, prd.grd, separableModel) 
        r <- raster(stfdf@sp[1])
        l <- length(stfdf@sp[1])
        for (n in 1:length(stfdf@time)){
            r@data@values[!is.na(r@data@values)] <- 
                stfdf@data$var1.pred[((n*l)-(l-1)):(n*l)]
            control <- addLayer(control, r)
        }
        for (p in 1:nlayers(control)) {
            t <- extract(control[[p]], pts[i])
            krT <- rbind(krT, t)
        }
    }
    ST_pred <- cbind(ST_pred, krT)
}

save(ST_pred, file="STK8_allprd.Rdata")

pred.LOO[[1]] <- NULL

pred.df <- c() 
for (p in 1:length(pts)) {
    e <- t(extract(pred.LOO[[p]], pts[p]))
    pred.df <- cbind(pred.df, e)
}
pred.res <- pred.df - value[2:length(value)]


d <- gDistance(data.s, byid=T)
dist <- c()
for (j in 1:nrow(data.s)){
    dist <- c(dist, mean(sort(d[,j])[2:4]))
}

d <- gDistance(data.s, kml, byid=T)
