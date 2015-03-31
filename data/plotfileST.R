load("STKE80_TempBrick.Rdata")
x <- calc(control, mean)
x <- disaggregate(x, fact=25, method="bilinear")
x <- crop(x, kml)
x <- mask(x, kml)
cols = rev(heat.colors(1000))
plot(x, col=cols, zlim=c(18, 22))
plot(kml, add=TRUE)

load("UK80_TempBrick.Rdata")
x2 <- calc(TempBrick, mean)
x2 <- disaggregate(x2, fact=25, method="bilinear")
x2 <- crop(x2, kml)
x2 <- mask(x2, kml)
plot(x2, col=cols, zlim=c(18, 22))
plot(kml, add=TRUE)

BrickU <- brick()
for (h in 1:24){
    BrickU <- addLayer(BrickU, calc(BrickUK[[seq(h, nlayers(BrickUK), 24)]], mean))
}

load("STKD8_TempBrick.Rdata")
TempBrick <- control
preddata <- c()
temp <- c()
MeanT <- brick()
for (d in 1:7){
    e <- d*24
    b <- e-23
    T <- calc(TempBrick[[seq(b, e, 1)]], mean)
    #T <- disaggregate(T, fact=10, method="bilinear")
    preddata <- c(preddata, extract(T, pts[1:80]))
    MeanT <- addLayer(MeanT, T)
    temp <- c(temp, colMeans(temp.cor[b:e, 2:81]))
}

res <- temp-preddata
RMSE <- sqrt(mean(res^2, na.rm=TRUE))
    
    
x <- colMeans(temp.cor[b:e, 2:81])

## 20 sensor dataset
#OK   = 0.23
#UK   = 0.20
#AK   = 0.19
#DK   = 0.20
#ADK  = 0.19
#CK   = 0.10
#CKR  = 0.10
#CKS  = 0.10
#STK  = 0.25
#STKE = 0.24
#STKD = 0.84

## 40 sensor dataset
#OK   = 0.23
#UK   = 0.20
#AK   = 0.19
#DK   = 0.20
#ADK  = 0.19
#CK   = 0.10
#CKR  = 0.10
#CKS  = 0.10
#STK  = 0.25
#STKE = 0.24
#STKD = 0.84


## 80 sensor dataset
#OK   = 0.29
#UK   = 0.26
#AK   = 0.26
#DK   = 0.26
#ADK  = 0.40
#RFK  = 0.40
#CK   = 0.14
#CKR  = 0.50
#CKS  = 0.14
#STK  = 0.14
#STKE = 0.14
#STKD = 0.35



seq(h, length(MPEs), 24)]

MPEsdf <- data.frame(MPEsm)

MPEsdf <- cbind(MPEsdf, MPEsm)

ggplot(MPEsdf, aes(x=1:24, y=MPEsdf[,1])) + geom_line() +
    geom_line(y=MPEsdf[,2], col="grey75", alpha=0.7, size=3) +
    geom_line(y=MPEsdf[,3], col="grey45", alpha=0.7, size=3) +
    geom_line(y=MPEsdf[,4], col="grey15", alpha=0.7, size=3) 
