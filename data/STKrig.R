control <- brick()
for (d in 1:7){
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = 2:ncol(temp)), 
                          time = temp[,1], SpatialObj = pts, interval = TRUE)
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


save(ST_pred, file="STKE80_allprd.Rdata")

ST_pred <- c()
for (i in 1:40){
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
        stfdf <- krigeST(values ~ log(altitude), st, prd.grd, separableModel) 
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
for (i in 1:20){
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
        stfdf <- krigeST(values ~ log(altitude), st, prd.grd, separableModel) 
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
save(ST_pred, file="STKE20_allprd.Rdata")

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
        stfdf <- krigeST(values ~ log(altitude), st, prd.grd, separableModel) 
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
save(ST_pred, file="STKE8_allprd.Rdata")

ST_pred <- c()
for (i in 1:80){
    krT <- c()
    for (d in 1:7){
        control <- stack()
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                          time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
        st@data$altitude <- rep(dem[-i], 24)
        st@data$slope <- rep(slp[-i], 24)
        st@data$aspect <- rep(asp[-i], 24)
        st@data$radiation <- as.vector(solx[,-i])
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
        stfdf <- krigeST(values ~ log(altitude) + radiation + log(aspect) +
                             log(slope), st, prd.grd, separableModel) 
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
save(ST_pred, file="STKR80_allprd.Rdata")

ST_pred <- c()
for (i in 1:40){
    krT <- c()
    for (d in 1:7){
        control <- stack()
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                          time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
        st@data$altitude <- rep(dem[-i], 24)
        st@data$slope <- rep(slp[-i], 24)
        st@data$aspect <- rep(asp[-i], 24)
        st@data$radiation <- as.vector(solx[,-i])
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
        stfdf <- krigeST(values ~ log(altitude) + radiation + log(aspect) +
                             log(slope), st, prd.grd, separableModel)  
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
save(ST_pred, file="STKR40_allprd.Rdata")

ST_pred <- c()
for (i in 1:20){
    krT <- c()
    for (d in 1:7){
        control <- stack()
        e <- d*24
        b <- e-23
        temp <- temp.cor[b:e,]
        st <- stConstruct(temp, space = list(temperature = (2:ncol(temp))[-i]), 
                          time = temp[, 1], SpatialObj = pts[-i], interval = TRUE)
        st@data$altitude <- rep(dem[-i], 24)
        st@data$slope <- rep(slp[-i], 24)
        st@data$aspect <- rep(asp[-i], 24)
        st@data$radiation <- as.vector(solx[,-i])
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
        stfdf <- krigeST(values ~ log(altitude) + radiation + log(aspect) +
                             log(slope), st, prd.grd, separableModel) 
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
save(ST_pred, file="STKR20_allprd.Rdata")

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
        st@data$altitude <- rep(dem[-i], 24)
        st@data$slope <- rep(slp[-i], 24)
        st@data$aspect <- rep(asp[-i], 24)
        st@data$radiation <- as.vector(solx[,-i])
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
        stfdf <- krigeST(values ~ log(altitude) + radiation + log(aspect) +
                             log(slope), st, prd.grd, separableModel)  
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
save(ST_pred, file="STKR8_allprd.Rdata")