day <- 31
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allres <-c()
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
        #allres <- c(allres, res)
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
        #allres <- c(allres, res)
        #LOO <- c(LOO, sqrt(mean(res^2)))
        #x <- raster(k.2@predicted)
        
        ## CO-KRIGING
            predgrd$radiation <- rad[[i]]
            radiation <- solx[i,]
            data.s$radiation <- radiation[points]
        g <- gstat(NULL, id="temperature", form=temperature~1, 
                                      data=data.s)
        g <- gstat(g, id="altitude", form=altitude~1, 
                                      data=data.s)
        g <- gstat(g, id="Radiation", form=radiation~1, 
               data=data.s)
        
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Sph", 1000))
        c.k <- predict(vg.fit, predgrd)
        x <- raster(c.k)
        x <- x$temperature.pred
        pe1 <- gstat.cv(vg.fit, all.residual=TRUE, verbose=FALSE)
        res <- pe1$temperature
        allres <- c(allres, res)
        LOO <- c(LOO, sqrt(mean((res)^2)))
        
        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(LOO, file="RMSE_CKR.Rdata")
save(allres, file="allres_CKR.Rdata")
save(TempBrick, file="Brick_CKR.Rdata")

day <- 31
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allres <-c()
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
        #allres <- c(allres, res)
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
        #allres <- c(allres, res)
        #LOO <- c(LOO, sqrt(mean(res^2)))
        #x <- raster(k.2@predicted)
        
        ## CO-KRIGING
        predgrd$radiation <- rad[[i]]
        radiation <- solx[i,]
        data.s$radiation <- radiation[points]
        g <- gstat(NULL, id="temperature", form=temperature~1, 
                   data=data.s)
        g <- gstat(g, id="altitude", form=altitude~1, 
                   data=data.s)
        #g <- gstat(g, id="Radiation", form=radiation~1, 
        #           data=data.s)
        
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Sph", 1000))
        c.k <- predict(vg.fit, predgrd)
        x <- raster(c.k)
        x <- x$temperature.pred
        pe1 <- gstat.cv(vg.fit, all.residual=TRUE, verbose=FALSE)
        res <- pe1$temperature
        allres <- c(allres, res)
        LOO <- c(LOO, sqrt(mean((res)^2)))
        
        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(LOO, file="RMSE_CK.Rdata")
save(allres, file="allres_CK.Rdata")
save(TempBrick, file="Brick_CK.Rdata")