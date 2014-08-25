### Function to create a single zoo-object from the *.csv files of micro-sensors


zoo.sensors <- function (dir = "/home/kees/ibuttons/Honduras/primera/humidity/", 
                        reg = "/home/kees/ibuttons/Honduras/ibuttons.csv", 
                        format = "%m-%d-%y %I:%M:%S %p", unit= "hours", na= 75,
                        na.estimate = na.spline, msr = "humidity", 
                        sensor = "iButton") {
                            
                                                            require(zoo)
                                                            require(lubridate)               
              
    if (sensor == "iButton") {
        rowskip = 19 
        time = "Date.Time" 
        val = "Value" 
    }
    
    if (msr == "humidity") {
        title <- "Diurnal huHmidity range (% RH)" 
        lab <- "Relative Humidity" 
        lim <- c(0, 100)
        u <- "%RH" 
        up = 100
        lo = 0
        max = 100
    }
    
    else {
        title <- "Diurnal temperature range (°C)" 
        lab <- "Degrees Celsius" 
        lim <- c(0, 50)
        u <- "°C." 
        up = 50
        lo = 10
        max = NA
    }
    
    f <- dir(dir, pattern = ".csv")      
        loc <<- read.csv(reg) 
            obs <- nrow(loc)     
                com <- which(!is.na(loc$X | loc$Y))
                    ok.loc <- length(com)
                        loc <- na.omit(loc) 
                            obs.zoo <- list()   
                           
    for (i in com) {
        v  <- subset(read.csv(paste(dir, f[i], sep =""), skip = rowskip, 
                              header = TRUE), select = c(time, val)) 
            v$Date.Time <- strptime(v$Date.Time, format)
                vc <- as.character(v$Date.Time)
                    v$Date.Time <- as.POSIXct(vc)
                        v <- na.omit(v)                            
                            v.zoo <- zoo(v$Value, v$Date.Time)
                                obs.zoo[[i]] <- v.zoo
    }     
    
    obs.zoo <- obs.zoo[!sapply(obs.zoo, is.null)] 
        obs.df <- do.call(merge, obs.zoo)
            start <- ceiling_date(start(obs.df), unit = "hour")
                end <- ceiling_date(end(obs.df), unit = "hour") 
                    season <- seq(from = start, to = end, 
                        by = as.difftime(1, units = unit))  
                           index <- zoo(1, season) 
                                seas <<- index
    
    est.obs <- na.estimate(merge.zoo(index, obs.df), maxgap = length(com))
        est.obs <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))
            est.obs[est.obs < lo] <- NA                 
                est.obs[est.obs > up] <- max
                    est.obs[1] <- as.POSIXct(rownames(est.obs))
                        est.obs[2] <- NULL
                            names(est.obs) <- c("Date", as.character(loc$CODE))
    
    obs.ok <<- est.obs[, colMeans(!is.na(est.obs[2:ncol(est.obs)])) >= na/100]       
        obs.ok$ID13 <- NULL # This is an error, should be removed
            ok.col <-length(obs.ok[2:ncol(obs.ok)])
                tot.val <- sum(!is.na(est.obs))
                    bad.val <- length(which(est.obs > up | (est.obs < lo)))
                        na.val <- sum(is.na(est.obs))
                            obs.ok <<- obs.ok
                             obs.cor <<- na.omit(obs.ok)
    
      return(cat(obs, "Farmers have particiated in the study. Out of the total", 
           ok.loc, "have included complete XY coordinates and", ok.col, 
           "have < 25% NA.", tot.val, "values can be used, while", bad.val, 
           "are NA or outside the physical limits: <", lo, "and >", up, u))
}