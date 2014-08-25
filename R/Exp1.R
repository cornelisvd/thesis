    require(zoo)
    require(lubridate)  
    require(ggplot2)
    
    dir = "/media/kees/Rugged_Key/Thesis/Experiments/Exp1/Type1/Temp/"
    format = "%d/%m/%y %H:%M:%S" 
    sensor = "iButton"      
    unit= "mins"
    cl <- scale_colour_gradient2(high="blue")
    
    if (sensor == "iButton") {
        rowskip = 19 
        time = "Date.Time" 
        val = "Value" 
    }
    
    f <- dir(dir, pattern = ".csv")      
    obs.zoo <- list() 
     
    for (i in 1:length(f)) {
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
    obs.df2 <- obs.df[seq(1, NROW(obs.df), 59),]
    
    start <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S") 
    end <- as.POSIXct("30/06/14 11:00:00", format = "%d/%m/%y %H:%M:%S")
    season <- seq(from = start, to = end, 
                  by = as.difftime(1, units = "mins"))  
    index <- zoo(1, season) 
    seas <<- index
    
    est.obs <- na.spline(merge.zoo(index, obs.df2))
    est.obs <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))
    est.obs[1] <- as.POSIXct(rownames(est.obs))
    est.obs[2] <- NULL
    est.obs <- na.omit(est.obs)
    names(est.obs) <- c("Index", 1:length(f))
    
    #P <- list()
    #nplot <- 2:length(est.obs)
    #for (j in nplot) {
     #   P[[length(P) + 1]] <- print(ggplot(est.obs, aes(x=est.obs$Index, 
     #   y=est.obs[,j])) + geom_line(aes(colour = est.obs[,j]), alpha = 0.5) + 
     #   xlab("Date") + cl + #geom_hline(yintercept = th, colour = "blue") + 
     #   ylim(0,40) +
     #   geom_smooth(method = "auto", linetype="dashed", aes(group=1)))  
        
    }
    est.obs[1] <- NULL
    rm(i, j, v, P, cl, dir, end, f, format, index, obs.df, obs.zoo, rowskip,
       seas, season, sensor, start, time, unit, v.zoo, val, vc, nplot, obs.df2)
    