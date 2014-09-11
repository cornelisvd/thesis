exp.time3    <- function(exp      = "Exp2",
                         type     = "Station",
                         rowskip  =  19, 
                         time     = "Date.Time", 
                         val      = "Value",
                         format.m = "%d/%m/%y %H:%M:%S",
                         start.m  = "29/06/14 00:00:00",
                         end.m    = "29/06/14 23:59:00",
                         time.un  = "mins",
                         time.dif = 1,
                         unit     = "Temp",
                         na.est   = na.approx)
{
    
    ## 
    require(zoo)
    require(ggplot2)
    
    start    <- as.POSIXct(start.m, format = format.m)
    end      <- as.POSIXct(end.m, format = format.m)
    season   <<- seq(from = start, to = end, 
                     by = as.difftime(time.dif, units = time.un))  
    index    <- zoo(1, season) 
    seas     <- index
    
    ##    
    type.dir <- dir(paste(exp,"/",type,"/",unit, sep =""), pattern =".csv")
    type.zoo <- list() 
    
    ##
    for (i in 1:length(type.dir)) {
        v  <- subset(read.csv(paste(exp, "/", type, "/", unit, "/", 
                                    type.dir[i], sep =""), skip = rowskip, 
                              header = TRUE), select = c(time, val)) 
        v$Date.Time <- strptime(v$Date.Time, format.m)
        vc <- as.character(v$Date.Time)
        v$Date.Time <- as.POSIXct(vc)
        v <- na.omit(v)                            
        v.zoo <- zoo(v$Value, v$Date.Time)
        type.zoo[[i]] <- v.zoo
    }  
    
    ##
    type.zoo <- type.zoo[!sapply(type.zoo, is.null)] 
    type.df  <- do.call(merge, type.zoo)
    
    ## Interpolate points with NA values
    est.obs   <- na.est(merge.zoo(index, type.df))
    est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
    est.obs  <<- est.obs[3:(length(type.dir)+2)]
}
##--------------------------------------------------------------------------##

exp.time4    <- function(exp      = "Exp3",
                         type     = "Station",
                         rowskip  =  19, 
                         time     = "Date.Time", 
                         val      = "Value",
                         format.m = "%d/%m/%y %H:%M:%S",
                         start.m  = "03/07/14 00:00:00",
                         end.m    = "03/07/14 23:59:00",
                         time.un  = "hours",
                         time.dif = 1,
                         unit     = "Temp",
                         na.est   = na.spline)
{
  
  ## 
  require(zoo)
  require(ggplot2)
  
  start    <- as.POSIXct(start.m, format = format.m)
  end      <- as.POSIXct(end.m, format = format.m)
  season   <<- seq(from = start, to = end, 
                   by = as.difftime(time.dif, units = time.un))  
  index    <- zoo(1, season) 
  seas     <- index
  
  ##    
  type.dir <- dir(paste(exp,"/",unit,"/",type, sep =""), pattern =".csv")
  type.zoo <- list() 
  
  ##
  for (i in 1:length(type.dir)) {
    v  <- subset(read.csv(paste(exp, "/", unit, "/", type, "/", 
                                type.dir[i], sep =""), skip = rowskip, 
                          header = TRUE), select = c(time, val)) 
    v$Date.Time <- strptime(v$Date.Time, format.m)
    vc <- as.character(v$Date.Time)
    v$Date.Time <- as.POSIXct(vc)
    v <- na.omit(v)                            
    v.zoo <- zoo(v$Value, v$Date.Time)
    type.zoo[[i]] <- v.zoo
  }  
  
  ##
  type.zoo <- type.zoo[!sapply(type.zoo, is.null)] 
  type.df  <- do.call(merge, type.zoo)
  
  ## Interpolate points with NA values
  est.obs   <- na.est(merge.zoo(index, type.df))
  est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
  est.obs  <<- est.obs[3:(length(type.dir)+2)]
}
##--------------------------------------------------------------------------##