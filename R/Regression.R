##-----------------------------Sensor regression------------------------------##






##---------------------------------v.01-09-14---------------------------------##

library(splines)
library(quantreg)

# Set the working directory
setwd ("~/thesis/data/Experiments/")   

# Load libraries & clear the workspace
rm (list = ls(all = TRUE))  ;   
source(".PackageInstall.R")
source(".Exp2Functions.R")

t = 2/4
x = c(10, 15, 20, 25, 30, 35, 40)

##------------------------First test (station/pipes)--------------------------##

    exp.time3(type = "Station", unit = "Temp")
    test1.station.temp <- as.matrix(est.obs)
    exp.time3(type = "Pipes", unit = "Temp")
    test1.pipes.temp <- as.matrix(est.obs)
    t1.s.t <- rowMeans(test1.station.temp)
    t1.p.t <- rowMeans(test1.pipes.temp)

##------------------------2nd test (large tubes)------------------------------##

    exp.time4(exp = "Exp3", type = "Station", unit = "Temp")
    test2.station.temp <- as.matrix(est.obs)
    exp.time4(exp = "Exp3", type = "White", unit = "Temp")
    test2.white.temp <- as.matrix(est.obs)  
    t2.s.t <- rowMeans(test2.station.temp)
    t2.w.t <- rowMeans(test2.white.temp)

##-------------------------3rd test (large tubes)-----------------------------##

    exp.time4(exp = "Exp4", type = "Station", unit = "Temp", 
          start.m  = "06/07/14 00:00:00",
          end.m    = "06/07/14 23:59:00")
    test3.station.temp <- as.matrix(est.obs)
    exp.time4(exp = "Exp4", type = "White", unit = "Temp", 
          start.m  = "06/07/14 00:00:00",
          end.m    = "06/07/14 23:59:00")
    test3.white.temp <- as.matrix(est.obs)  
    t3.s.t <- rowMeans(test3.station.temp)
    t3.w.t <- rowMeans(test3.white.temp)

##---------------------------4th test (small tubes)---------------------------##

    exp.time4(exp = "Exp5", type = "Station", unit = "Temp", 
          start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
    test4.station.temp <- as.matrix(est.obs)
    exp.time4(exp = "Exp5", type = "Nothing", unit = "Temp", 
          start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
    test4.nothing.temp <- as.matrix(est.obs)  
    t4.s.t  <- rowMeans(test4.station.temp)
    t4.n.t  <- rowMeans(test4.nothing.temp)

##---------------------------5th test (small tubes)---------------------------##

    exp.time4(exp = "Exp6", type = "Station", unit = "Temp", 
          start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
    test5.station.temp <- as.matrix(est.obs)
    exp.time4(exp = "Exp6", type = "Nothing", unit = "Temp", 
              start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
    test5.nothing.temp <- as.matrix(est.obs)
    t5.s.t  <- rowMeans(test5.station.temp)
    t5.n.t  <- rowMeans(test5.nothing.temp)

##---------------------------Quantile regression------------------------------##

    stationdata <- c(t1.s.t, t2.s.t, t3.s.t, t4.s.t, t5.s.t)
    
    pipedata    <- c(t1.p.t, t2.w.t, t3.w.t, t4.n.t, t5.n.t)
    
    comp <- data.frame(stationdata, pipedata)
    
          
    plot(comp$pipedata,comp$stationdata,xlab = "Shield temperature", 
         ylab = "Temperature in weather station",type="n")
    points(comp$pipedata,comp$stationdata,cex = 0.2, pch = 19, col = "grey75")
    X <- model.matrix(comp$stationdata ~ bs(comp$pipedata))
     tau <- c(0.75)

     fit <- rq(stationdata ~ bs(pipedata), tau=tau, data=comp)
     accel.fit <- X %*% fit$coef
     lines(comp$pipedata,accel.fit, lty="dotted")

      new.df <- data.frame(pipedata = x)
      p <- predict(fit, new.df)
    comp2 <- data.frame(stationdata, p)
    save(fit, file = "fit.Rdata")
