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
        # ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~log(altitude),#log(altitude), # or 1
        #               data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
        ##allres <- c(allres, res)
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
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)

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
        #      radiation <- solx[i,]
        #    data.s$radiation <- radiation[points]
        #    kri <- autoKrige(temperature ~ log(altitude) + log(radiation) +
        #                         log(slope) + log(aspect),
        #                     data.s, predgrd)
        #}    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        #kriging.pred <- kri$krige_output
        #x <- raster(kriging.pred)
        #cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        #act <- cv$krige.cv@data$observed
        #prd <- cv$krige.cv@data$var1.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
        #res2 <- extract(x, data.c)
        #res3 <- (data.c[[i]] - res2)^2
        #RMSE <- c(RMSE, sqrt(mean(res3)))
        
        ## FIT.GSTATMODEL
        #radiation <- solx[i,]
        #data.s$radiation <- radiation[points]
        #predgrd$radiation <- rad[[i]]
        #fit <- fit.gstatModel(data.s, temperature~altitude+radiation+
        #                          slope+aspect,
        #                          family = gaussian(log),
        #                          predgrd, 
        #                          method="randomForest")
        #k.2 <- predict(fit, predgrd, nfold=length(data.s))
        #x <- raster(k.2@predicted)
        #act <- k.2@validation@data$observed
        #prd <- k.2@validation@data$var1.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
        #LOO <- c(LOO, sqrt(mean(res^2)))
        #x <- raster(k.2@predicted)
        
        ## CO-KRIGING
        #if (i <= 5 | i >= 18){
            g <- gstat(NULL, id="Temperature", form=temperature~1, 
                       data=data.s)
            g <- gstat(g, id="Altitude", form=altitude~1, 
                       data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #               data=data.s)
        #    g <- gstat(g, id="Aspect", form=aspect~1, 
        #               data=data.s)
        #} else {
            predgrd$radiation <- rad[[i]]
            radiation <- solx[i,]
            data.s$radiation <- radiation[points]
        #    g <- gstat(NULL, id="Temperature", form=temperature~1, 
        #                              data=data.s)
        #    g <- gstat(g, id="Altitude", form=altitude~1, 
        #                              data=data.s)
            g <- gstat(g, id="Radiation", form=radiation~1, 
               data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #           data=data.s)
        #}
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Exp", 250))
        c.k <- predict(vg.fit, predgrd)
        pe1 <- gstat.cv(vg.fit)
        act <- pe1@data$observed
        prd <- pe1@data$Temperature.pred
        allact <- c(allact, act)
        allprd <- c(allprd, prd)
        x <- raster(c.k)
        x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
        
        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(TempBrick, file="CKR80_TempBrick.Rdata")
save(allact, file="CKR80_allact.Rdata")
save(allprd, file="CKR80_allprd.Rdata")

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
        # ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~log(altitude),#log(altitude), # or 1
        #           data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #    kri <- autoKrige(temperature ~ log(altitude) + log(radiation) +
        #                         log(slope) + log(aspect),
        #                     data.s, predgrd)
        #}    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        #kriging.pred <- kri$krige_output
        #x <- raster(kriging.pred)
        #cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        #act <- cv$krige.cv@data$observed
        #prd <- cv$krige.cv@data$var1.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #if (i <= 5 | i >= 18){
        g <- gstat(NULL, id="Temperature", form=temperature~1, 
                   data=data.s)
        g <- gstat(g, id="Altitude", form=log(altitude)~1, 
                   data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #               data=data.s)
        #    g <- gstat(g, id="Aspect", form=aspect~1, 
        #               data=data.s)
        #} else {
        #    predgrd$radiation <- rad[[i]]
        #    radiation <- solx[i,]
        #    data.s$radiation <- radiation[points]
        #    g <- gstat(NULL, id="Temperature", form=temperature~1, 
        #                              data=data.s)
        #    g <- gstat(g, id="Altitude", form=altitude~1, 
        #                              data=data.s)
        #    g <- gstat(g, id="Radiation", form=radiation~1, 
        #      data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #           data=data.s)
        #}
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Exp", 250))
        c.k <- predict(vg.fit, predgrd)
        pe1 <- gstat.cv(vg.fit)
        act <- pe1@data$observed
        prd <- pe1@data$Temperature.pred
        allact <- c(allact, act)
        allprd <- c(allprd, prd)
        x <- raster(c.k)
        x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
        
        
        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(TempBrick, file="CK80_TempBrick.Rdata")
save(allact, file="CKR80_allact.Rdata")
save(allprd, file="CKR80_allprd.Rdata")

day <- 7
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allact <- c()
allprd <- c()
points <- seq(1:20)
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
        # ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~log(altitude),#log(altitude), # or 1
        #           data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
        #allres <- c(allres, res)
        #LOO <- c(LOO, sqrt(mean((res)^2)))
        
        #res <- extract(x, data.c)
        #res <- (data.c[[i]] - res)^2
        #RMSE <- c(RMSE, sqrt(mean(res)))
        
        ## DYNAMIC UNIVERSAL KRIGING
         #if (i <= 5 | i >= 18){
         #      g <- gstat(NULL, id="temperature", form=temperature~log(altitude)+
         #                     log(slope)+log(aspect), # or 1
         #          data=data.s)
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
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #    kri <- autoKrige(temperature ~ log(altitude) + log(radiation) +
        #                         log(slope) + log(aspect),
        #                     data.s, predgrd)
        #}    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        #kriging.pred <- kri$krige_output
        #x <- raster(kriging.pred)
        #cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        #act <- cv$krige.cv@data$observed
        #prd <- cv$krige.cv@data$var1.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #if (i <= 5 | i >= 18){
        g <- gstat(NULL, id="Temperature", form=temperature~1, 
                   data=data.s)
        g <- gstat(g, id="Altitude", form=altitude~1, 
                   data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #               data=data.s)
        #    g <- gstat(g, id="Aspect", form=aspect~1, 
        #               data=data.s)
        #} else {
            predgrd$radiation <- rad[[i]]
            radiation <- solx[i,]
            data.s$radiation <- radiation[points]
        #    g <- gstat(NULL, id="Temperature", form=temperature~1, 
        #                              data=data.s)
        #    g <- gstat(g, id="Altitude", form=altitude~1, 
        #                              data=data.s)
            g <- gstat(g, id="Radiation", form=radiation~1, 
               data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #           data=data.s)
        #}
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Exp", 250))
        c.k <- predict(vg.fit, predgrd)
        pe1 <- gstat.cv(vg.fit)
        act <- pe1@data$observed
        prd <- pe1@data$Temperature.pred
        allact <- c(allact, act)
        allprd <- c(allprd, prd)
        x <- raster(c.k)
        x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
        
        
        
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(TempBrick, file="CKR20_TempBrick.Rdata")
save(allact, file="CKR20_allact.Rdata")
save(allprd, file="CKR20_allprd.Rdata")

day <- 7
TempBrick <- brick()
TempList <- list()
LOO  <- c()
RMSE <- c()
allact <- c()
allprd <- c()
points <- c(67:74)
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
        # ORDINARY / BASIC UNIVERSAL KRIGING
        #g <- gstat(NULL, id="temperature", form=temperature~log(altitude),#log(altitude), # or 1
        #           data=data.s)
        #var2 <- variogram(g)
        #var3 <- fit.variogram(var2, vgm(1, "Exp", 250))
        #g <- gstat(g, model=var3, fill.all=TRUE)
        #k.c <- predict.gstat(g, predgrd)
        #x <- raster(k.c)
        #x <- x$temperature.pred
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        # radiation <- solx[i,]
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
        #pe1 <- gstat.cv(g)
        #act <- pe1@data$observed
        #prd <- pe1@data$temperature.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #    kri <- autoKrige(temperature ~ log(altitude) + log(radiation) +
        #                         log(slope) + log(aspect),
        #                     data.s, predgrd)
        #}    
        #kri <- autoKrige(temperature ~ log(altitude),
        #                 data.s, predgrd)
        #kriging.pred <- kri$krige_output
        #x <- raster(kriging.pred)
        #cv <- autoKrige.cv(temperature ~ altitude, data.s, nfold=length(data.s))
        #act <- cv$krige.cv@data$observed
        #prd <- cv$krige.cv@data$var1.pred
        #allact <- c(allact, act)
        #allprd <- c(allprd, prd)
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
        #if (i <= 5 | i >= 18){
        g <- gstat(NULL, id="Temperature", form=temperature~1, 
                   data=data.s)
        g <- gstat(g, id="Altitude", form=altitude~1, 
                   data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #               data=data.s)
        #    g <- gstat(g, id="Aspect", form=aspect~1, 
        #               data=data.s)
        #} else {
            predgrd$radiation <- rad[[i]]
            radiation <- solx[i,]
            data.s$radiation <- radiation[points]
        #    g <- gstat(NULL, id="Temperature", form=temperature~1, 
        #                              data=data.s)
        #    g <- gstat(g, id="Altitude", form=altitude~1, 
        #                              data=data.s)
            g <- gstat(g, id="Radiation", form=radiation~1, 
               data=data.s)
        #    g <- gstat(g, id="Slope", form=slope~1, 
        #           data=data.s)
        #}
        vg <- gstat::variogram(g)
        vg.fit <- fit.lmc(vg, g, vgm(1, "Exp", 250))
        c.k <- predict(vg.fit, predgrd)
        pe1 <- gstat.cv(vg.fit)
        act <- pe1@data$observed
        prd <- pe1@data$Temperature.pred
        allact <- c(allact, act)
        allprd <- c(allprd, prd)
        x <- raster(c.k)
        x <- x$temperature.pred
        #LOO <- c(LOO, sqrt(mean(gstat.cv(g, all.residual=TRUE, verbose=FALSE)^2)))
                
        #x <- disaggregate(x, fact=10, method="bilinear")
        x <- mask(x, kml)
        TempList[[n]] <- x
    }
}
TempBrick <- brick(TempList)
proc.time() - ptm

save(TempBrick, file="CKR80_TempBrick.Rdata")
save(allact, file="CKS80_allact.Rdata")
save(allprd, file="CKS80_allprd.Rdata")

load("CKR8_TempBrick.Rdata")
load("CKR8_allprd.Rdata")
load("CKR8_allact.Rdata")
allprd <- matrix(allprd, ncol=80)
allact <- matrix(allact, ncol=80)
allres <- allact - allprd

RMSE <- sqrt(mean(allres^2, na.rm=TRUE))

load("STKR8_allprd.Rdata")
allprd <- ST_pred
allact <- as.matrix(temp.cor[1:168,2:9])
allres <- allact - ST_pred

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

MPEt <- c()
for (c in 1:ncol(allprd)){
   MPEt <- c(MPEt, mean(abs(allprd[,c]-allact[,c])/allact[,c])*100)
}

MPEs <- c()
for (r in 1:nrow(allprd)){
    MPEs <- c(MPEs, mean(abs(allprd[r,]-allact[r,])/allact[r,])*100)
}

load("AK8_TempBrick.Rdata")
x <- calc(TempBrick, mean)
x <- disaggregate(x, fact=10, method="bilinear")
cols <- rev(heat.colors(100))
plot(x, col=cols)
plot(kml, add=TRUE)


ST_pred <- c()
for (i in 1:40)){
    krT <- c()
    for (d in 1:7){
        control <- stack()
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                          time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
        st@data$altitude <- rep(dem[-i], 24)
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
        stfdf <- krigeST(values ~ altitude, st, prd.grd, separableModel) 
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
