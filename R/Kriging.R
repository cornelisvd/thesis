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
vegetation <- projectRaster(LAI, crs=projection(kml))
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
solx <- rbind(solx, solx, solx, solx, solx, solx, solx)

d <- 31
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allres <-c()
points <- seq(1:80)
    #c(67:74)
ptm <- proc.time()
    for (i in ((21*24)+1):(24*d)){
        temperature <- temp[,i]
        data.f$temperature <- temperature
        data.s  <- data.f[points,]
        #data.c  <- data.f[-points,]
        IDW1n_fit <- gstat(id="IDW1n_fit", formula = temperature ~ 1, data = data.s,
                           set=list(idp=2))
        pe1 <- gstat.cv(IDW1n_fit, nfold=length(data.s))
        res <- pe1@data$residual
        allres <- c(allres, res)
        RMSE <- c(RMSE, sqrt(mean(res^2)))
## ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~log(altitude), # or 1
        #               data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
        #res <- extract(x, data.c)
        #res <- (data.c[[i]] - res)^2
        #RMSE <- c(RMSE, sqrt(mean(res)))

## DYNAMIC UNIVERSAL KRIGING
        #  if (i <= 5 | i >= 18){
        #  g <- gstat(NULL, id="Temperature", form=temperature~log(altitude)+
        #                  log(slope)+log(aspect), data=data.s)
        #} else {
        #    predgrd$radiation <- rad[[i]]
        #    radiation <- solx[i,]
        #    data.s$radiation <- radiation[points]
        #    g <- gstat(NULL, id="Temperature", form=temperature~log(altitude)+
        #                   log(radiation), 
        #                   data=data.s)
        #}
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Sph", 1000))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
        #res <- extract(x, data.c)
        #res <- (data.c[[i]] - res)^2
        #RMSE <- c(RMSE, sqrt(mean(res)))
        
## AUTOMAP KRIGING
        #if (i <= 5 | i >= 18){
        #    kri <- autoKrige(temperature ~ log(altitude) + log(slope) +
        #                         log(aspect),
        #                     data.s, predgrd)
        #} else {
        #    predgrd$radiation <- rad[[i]]
        #    radiation <- solx[i,]
        #    data.s$radiation <- radiation[points]
        #    kri <- autoKrige(temperature ~ log(altitude), + log(radiation),
        #                     data.s, predgrd)
        #}    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        #kriging.pred <- kri$krige_output
        #x <- raster(kriging.pred)
        #cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        #res <- cv$krige.cv@data$residual
        #LOO <- c(LOO, sqrt(mean(res^2)))
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
        #x <- mask(x, kml)
        #TempList[[i]] <- x
    }
    TempBrick <- brick(TempList)
proc.time() - ptm


DK.LOO <- matrix(LOO, ncol=7)
#save(DK.LOO, file="DK.LOO.Rdata")
UK.LOO <- matrix(LOO, ncol=7)
#save(DK.LOO, file="UK.LOO.Rdata")

OK.LOO <- matrix(LOO, ncol=7)
#save(DK.LOO, file="OK.LOO.Rdata")
RF.LOO <- matrix(LOO, ncol=7)
#save(DK.LOO, file="RF.LOO.Rdata")
AK.LOO <- matrix(LOO, ncol=7)
save(AK.LOO, file="AK.LOO.Rdata")

x <- data.frame(rowMeans(OK.LOO), rowMeans(OK.LOO_MF), rowMeans(UK.LOO))
ggplot(data=x, aes(x=1:24, y=x[,1])) + theme_set(theme_gray(base_size = 18)) +
    geom_line(aes(colour="Auto fit OK"), type="dotted") +
    geom_line(aes(y=x[,2], colour="Manual fit OK"), linetype="dashed") +
    geom_line(aes(y=x[,3], colour="Universal Kriging"), linetype="longdash") +
    scale_colour_manual(name="Interpolation Approach",
                        values=c("black","red","blue")) +
    xlab("Hour") + ylab("RMSE") + coord_cartesian(xlim=c(1, 24))


temp.cor <- temp.cor[1:24,]
predgrd2 <- as(predgrd, "SpatialPixels")
st <- stConstruct(temp.cor, space = list(temperature = 2:ncol(temp.cor)), 
                  time = temp.cor[, 1], SpatialObj = pts, interval = TRUE)
st@data$altitude <- rep(dem, 24)

tgrd <- seq(min(index(st)), max(index(st)), length = nrow(temp.cor))
prd.grd = STF(predgrd, tgrd)

### Select the parameters for the actual interpolation
v <-  vgmST("separable", 
            space = vgm(1, "Exp", 250*24), 
            time  = vgm(1, "Lin", 24 * 60), 
            sill  = 1)

### Create the actual STFDF object and add to list
stfdf <- krigeST(values ~ altitude, st, prd.grd, v) 

# step1 <- spTransform data.s object
# step2 <- brick the nona object, projectRaster and do 'as SpatialPixelsDF'

d <- gDistance(data.s, byid=T)
dist <- c()
for (j in 1:nrow(data.s)){
    dist <- c(dist, mean(sort(d[,j])[2:4]))
}

d <- gDistance(data.s, kml, byid=T)
