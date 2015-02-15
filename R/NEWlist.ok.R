setwd ("~/thesis/data/Experiments/") 
load("AquiaresData.Rdata")

    data2[,1] <- as.POSIXct(data2[,1], format="%Y-%m-%d %H:%M:%S")
    data3 <- subset(data2, data2$TIMESTAMP > as.POSIXct("2014-08-01") & 
                                    data2$TIMESTAMP <= as.POSIXct("2014-08-31"))
    tempx <- as.numeric(as.character(data3$AirTC_Avg))
    humdx <- as.numeric(as.character(data3$RH))
    lmodel <- lm(humdx~tempx)

#load("temperature.Rdata")
est.obs[,1] <- as.POSIXct(rownames(est.obs))
est.obs <- xxx

x <- as.matrix(est.obs[2:ncol(est.obs)])
x[x > quantile(x, 0.98, na.rm=TRUE)] <- quantile(x, 0.98, na.rm=TRUE)
x <- as.data.frame(x)
est.obs <- cbind(est.obs[,1], x)
names(est.obs[,1]) <- "Hour"
list.ok <- list()
list.ok$temperature <- est.obs



    namesdf <- names(est.obs)
    humidityx <- c()
    time <- est.obs[,1]
    est.obs <- est.obs[,-1]

    for (i in 1:ncol(est.obs)){
    humidityx <- cbind(humidityx, predict(lmodel, data.frame(temp=est.obs[,i])))
    }
    humidityx[humidityx > 100] <- 100
    humidityx <- data.frame(time, humidityx)
    names(humidityx) <- namesdf

list.ok$humidity <- humidityx

