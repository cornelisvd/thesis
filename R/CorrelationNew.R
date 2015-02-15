library(matrixStats)

temp.cor <- list.cor[[1]]
temp.cor <- subset(temp.cor, temp.cor[,1] > as.POSIXct("2014-08-07") & 
                      temp.cor[,1] <= as.POSIXct("2014-09-07"))
temp <- as.matrix(temp.cor[,2:length(temp.cor)])

humd.cor <- list.cor[[2]]
humd.cor <- subset(humd.cor, humd.cor[,1] > as.POSIXct("2014-08-07") & 
                       humd.cor[,1] <= as.POSIXct("2014-09-07"))
humd <- as.matrix(humd.cor[,2:length(humd.cor)])

Temperature <- c()
for (n in 1:24){
    for (i in seq(24, nrow(temp), 24)){
    Temperature <- c(Temperature, temp[i,])
}
calc.relimp(cor, type="lmg", rank=TRUE, rela=TRUE)
}
Temperature <- matrix(Temperature, nrow=80)
#Temperature <- colMeans(Temperature)
    
    
Temperature <- c()
for (i in 1:ncol(temp)){
    Temperature <- c(Temperature, mean(temp[,i]))
    #Temperature <- c(Temperature, quantile(temp[,i], 0.9))
  }

## Load DEM with ShadowMap.R
load("pts.Rdata")
alt <- extract(dem, pts, buffer = 10, fun = mean)
Altitude   <- rep(alt, 31)
Vegetation <- rep(veg, 31)
Canopy     <- rep(can, 31)
Slope      <- rep(slp, 31)
Aspect     <- rep(asp, 31)
Radiation  <- rep(solx[17,], 31)
Radiation <- rep(sol, 31)

cor.altT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), alt)
    cor.altT <- append(cor.altT, c)
}

Altitude <- alt
Slope <- slp
Aspect <- asp
LAI <- veg
Canopy <- can
Radiation <- solx
rel <- c()
rsq <- c()
    for (d in 1:31){
        for (h in 1:24){
        i <- h+(d-1)*24
        #print(i)
        if (h <= dl[1] | h >= dl[2]){
          cor <- lm(as.numeric(temp[i,])~Altitude+Slope+Aspect+LAI+Canopy)
          x <- summary(cor)
          rsq <- c(rsq, x$r.squared)
          c <- calc.relimp(cor, type="lmg", rank=TRUE, rela=TRUE)
          rel <- rbind(rel, c(c$lmg, 0))
       } else {
         cor <- lm(as.numeric(temp[i,])~Altitude+Slope+Aspect+LAI+Canopy+Radiation[h,])
         x <- summary(cor)
         rsq <- c(rsq, x$r.squared)
         c <- calc.relimp(cor, type="lmg", rank=TRUE, rela=TRUE)
         rel <- rbind(rel, c$lmg)
       }  
}
}

rela <- as.data.frame(colMeans(rel))
names(rela) <- c("Altitude", "Slope", "Aspect", "LAI", "Canopy", "Radiation")    
ggplot(rela, aes(x=rela[,1]), y=names(rela)) + geom_bar(binwidth = x)

cor.altT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), alt)
    cor.altT <- append(cor.altT, c)
}


corH <- c()
for (n in 1:24){
    cor.altTh <- c()
for (j in seq(n, nrow(temp), 24)) {
    c <- cor(as.numeric(temp[j,]), alt)
    cor.altTh <- append(cor.altTh, c)
}
corH <- cbind(corH, cor.altTh)
}


quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(corH)){
    quantmin <- c(quantmin, quantile(corH[,i], 0.25))
    quantmean <- c(quantmean, mean(corH[,i]))
    quantmax <- c(quantmax, quantile(corH[,i], 0.75))
}
library(ggplot2)

corH <- data.frame(1:24, quantmean, quantmin, quantmax)

ggplot(corH, aes(x=corH[,1], y=corH[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corH[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corH[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")

cor.altRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), alt)
    cor.altRH <- append(cor.altRH, c)
}

corRH <- c()
for (n in 1:24){
    cor.altRHh <- c()
    for (j in seq(n, nrow(humd), 24)) {
        c <- cor(as.numeric(humd[j,]), alt)
        cor.altRHh <- append(cor.altRHh, c)
    }
    corRH <- cbind(corRH, cor.altRHh)
}

LAI  <- raster("/home/kees/thesis/data/GIS/LAI_new_UTM17N_2m_geotif.tif") 
LAI <- projectRaster(LAI, crs=projection(kml))
veg <- extract(LAI, pts, buffer = 10, fun = mean)

cor.vegT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), veg)
    cor.vegT <- append(cor.vegT, c)
}

cor.vegT2 <- c()
for (n in 1:24){
    cor.altRHh <- c()
    for (j in seq(n, nrow(temp), 24)) {
        c <- cor(as.numeric(temp[j,]), veg)
        cor.altRHh <- append(cor.altRHh, c)
    }
    cor.vegT2 <- cbind(cor.vegT2, cor.altRHh)
}

quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(cor.vegT2)){
    quantmin <- c(quantmin, quantile(cor.vegT2[,i], 0.25))
    quantmean <- c(quantmean, mean(cor.vegT2[,i]))
    quantmax <- c(quantmax, quantile(cor.vegT2[,i], 0.75))
}
library(ggplot2)

corV <- data.frame(1:24, quantmean, quantmin, quantmax)

ggplot(corV, aes(x=corV[,1], y=corV[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corV[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corV[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")

cor.vegRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), veg)
    cor.vegRH <- append(cor.vegRH, c)
}

cor.vegRH2 <- c()
for (n in 1:24){
    cor.altRHh <- c()
    for (j in seq(n, nrow(humd), 24)) {
        c <- cor(as.numeric(humd[j,]), veg)
        cor.altRHh <- append(cor.altRHh, c)
    }
    cor.vegRH2 <- cbind(cor.vegRH2, cor.altRHh)
}


lidara1 <- read.table("/media/kees/UUI/turrialba.lce.txt")
lidara1 <- lidara1[,-1]
lidara1 <- lidara1[,-1]
lidara1 <- lidara1[,-1]
lidara1 <- lidara1[,-4]
lidara1 <- lidara1[,-4]
lidara1 <- lidara1[,-4]
lidara1 <- lidara1[,-4]
lidara2 <- read.table("/media/kees/UUI/turrialba_lce.txt")
lidara2 <- lidara2[,-1]
lidara2 <- lidara2[,-1]
lidara2 <- lidara2[,-1]
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
lidar1 <- crop(lidar1, kml)
lidar2 <- crop(lidar2, kml)
lidar1 <- disaggregate(lidar1, fact = 10, method = "bilinear")
lidar2 <- disaggregate(lidar2, fact = 10, method = "bilinear")
height <- lidar2 - lidar1

can <- extract(height, pts, buffer = 10, fun = mean)

cor.canT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), can, use = "complete.obs")
    cor.canT <- append(cor.canT, c)
}

cor.canRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), can, use = "complete.obs")
    cor.canRH <- append(cor.canRH, c)
}

cor.canT2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(temp), 24)) {
        c <- cor(as.numeric(temp[j,]), can, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.canT2 <- cbind(cor.canT2, cor.canHhour)
}

quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(cor.canT2)){
    quantmin <- c(quantmin, quantile(cor.canT2[,i], 0.25))
    quantmean <- c(quantmean, mean(cor.canT2[,i]))
    quantmax <- c(quantmax, quantile(cor.canT2[,i], 0.75))
}
library(ggplot2)

corC <- data.frame(1:24, quantmean, quantmin, quantmax)

ggplot(corC, aes(x=corC[,1], y=corC[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corC[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corC[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")

cor.canRH2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(humd), 24)) {
        c <- cor(as.numeric(humd[j,]), can, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.canRH2 <- cbind(cor.canT2, cor.canHhour)
}



slope    <-  terrain(dem, opt='slope')
aspect   <-  terrain(dem, opt='aspect')
slp <- extract(slope, pts, buffer = 10, fun = mean)
asp <- extract(aspect, pts, buffer = 10, fun = mean)

cor.slpT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), slp)
    cor.slpT <- append(cor.slpT, c)
}

cor.slpT2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(temp), 24)) {
        c <- cor(as.numeric(temp[j,]), slp, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.slpT2 <- cbind(cor.slpT2, cor.canHhour)
}

library(ggplot2)

quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(cor.slpT2)){
    quantmin <- c(quantmin, quantile(cor.slpT2[,i], 0.25))
    quantmean <- c(quantmean, mean(cor.slpT2[,i]))
    quantmax <- c(quantmax, quantile(cor.slpT2[,i], 0.75))
}
library(ggplot2)

corS <- data.frame(1:24, quantmean, quantmin, quantmax)

ggplot(corS, aes(x=corS[,1], y=corS[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corS[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corS[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")

cor.slpRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), slp)
    cor.slpRH <- append(cor.slpRH, c)
}

cor.slpRH2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(humd), 24)) {
        c <- cor(as.numeric(humd[j,]), slp, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.slpRH2 <- cbind(cor.slpRH2, cor.canHhour)
}

cor.slpRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), slp)
    cor.slpRH <- append(cor.slpRH, c)


cor.aspT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), asp)
    cor.aspT <- append(cor.aspT, c)
}

cor.aspT2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(temp), 24)) {
        c <- cor(as.numeric(temp[j,]), asp, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.aspT2 <- cbind(cor.aspT2, cor.canHhour)
}

quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(cor.aspT2)){
    quantmin <- c(quantmin, quantile(cor.aspT2[,i], 0.25))
    quantmean <- c(quantmean, mean(cor.aspT2[,i]))
    quantmax <- c(quantmax, quantile(cor.aspT2[,i], 0.75))
}
library(ggplot2)

corA <- data.frame(1:24, quantmean, quantmin, quantmax)

ggplot(corA, aes(x=corA[,1], y=corA[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corA[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corA[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")


cor.aspRH <- c()
for (i in 1:nrow(humd)) {
    c <- cor(as.numeric(humd[i,]), asp)
    cor.aspRH <- append(cor.aspRH, c)
}

cor.aspRH2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(humd), 24)) {
        c <- cor(as.numeric(humd[j,]), asp, use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.aspRH2 <- cbind(cor.aspRH2, cor.canHhour)
}

sol <- extract(sun, pts, buffer = 10, fun = mean)

solx <- c()
for (l in 1:length(sun@data@names)){
    solx <- append(solx, extract(sun[[l]], pts, buffer = 10, fun = mean))
}
solx <- t(matrix(solx, nrow=80))

cor.solT <- c()
for (i in 1:nrow(temp)) {
    c <- cor(as.numeric(temp[i,]), sol)
    cor.solT <- append(cor.solT, c)
}


cor.solT2 <- c()
for (n in 1:24){
    cor.canHhour <- c()
    for (j in seq(n, nrow(temp), 24)) {
        c <- cor(as.numeric(temp[j,]), solx[n,], use = "complete.obs")
        cor.canHhour <- append(cor.canHhour, c)
    }
    cor.solT2 <- cbind(cor.solT2, cor.canHhour)
}

quantmin <- c()
quantmax <- c()
quantmean <- c()
for (i in 1:ncol(cor.solT2)){
    quantmin <- c(quantmin, quantile(cor.solT2[,i], 0.25, na.rm=TRUE))
    quantmean <- c(quantmean, mean(cor.solT2[,i]))
    quantmax <- c(quantmax, quantile(cor.solT2[,i], 0.75, na.rm=TRUE))
}
library(ggplot2)

corA <- data.frame(1:24, colMeans(cor.solT2), quantmin, quantmax)

ggplot(corA, aes(x=corA[,1], y=corA[,2])) + geom_line(colour="black", size = 1, alpha = 0.35) +
    theme_set(theme_grey(base_size = 18)) +
    geom_line(aes(y=corA[,3]), colour = "blue", size = 1, alpha = 0.35) +
    geom_line(aes(y=corA[,4]), colour = "red", size = 1, alpha = 0.35) +
    coord_cartesian(xlim = c(1, 24)) + geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    xlab("Hour") + ylab("Correlation coefficient")

mp <- barplot(colMeans(rel), xlab="Covariates", ylab="Relative Importance", ylim=c(0,0.8))
ma <- matrix(colMeans(rel))
y.means <- apply(rel,2,mean)
y.sd <- apply(rel,2,sd)
error.bar(mp,y.means, 1.96*y.sd/10)
