BrickU <- brick()
for (h in 1:24){
    BrickU <- addLayer(BrickU, calc(BrickUK[[seq(h, nlayers(BrickUK), 24)]], mean))
}

seq(h, length(MPEs), 24)]

MPEsdf <- data.frame(MPEsm)

MPEsdf <- cbind(MPEsdf, MPEsm)

ggplot(MPEsdf, aes(x=1:24, y=MPEsdf[,1])) + geom_line() +
    geom_line(y=MPEsdf[,2], col="grey75", alpha=0.7, size=3) +
    geom_line(y=MPEsdf[,3], col="grey45", alpha=0.7, size=3) +
    geom_line(y=MPEsdf[,4], col="grey15", alpha=0.7, size=3) 
